-- | Emit a function-level call graph with resolved callee modules.
--
-- For every value, constructor and type reference in the given source files this
-- reports the enclosing definition it occurs in and the module the referenced
-- name /originates/ from (following re-export chains), which is what makes the
-- output usable as a cross-module call graph rather than a per-file symbol dump.
module Scripts.CallGraph (
  CallGraphOptions (..),
  runCallGraph,
) where

import AST qualified
import AST.Haskell
import Arborist.Config (allSourceRoots, loadArboristConfig)
import Arborist.Files (buildModuleFileMap)
import Arborist.ProgramIndex (ProgramIndex)
import Arborist.Renamer (
  RenamePhase,
  ResolvedConstructor (..),
  ResolvedName (..),
  ResolvedVariable (..),
  renamePrg,
 )
import Arborist.Scope.Global (ExportIndex, getExportedDecls)
import Arborist.Scope.Types (
  GlblConstructorInfo (..),
  GlblNameInfo (..),
  GlblVarInfo (..),
  ResolvedVarInfo (..),
 )
import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.HashMap.Lazy qualified as Map
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Pos (Pos (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HaskellAnalyzer (parsePrg)
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir
import System.Directory qualified as Dir
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

data CallGraphOptions = CallGraphOptions
  { sourceFiles :: [FilePath]
  , filesFrom :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , configFile :: FilePath
  , includeTypes :: Bool
  , quiet :: Bool
  }

-- | A definition we attribute references to.
data Def = Def
  { name :: Text
  , mod :: Text
  , line :: Int
  , instanceHead :: Maybe Text
  -- ^ Instance head when this is an instance method binding, e.g.
  -- @HasSubmitFee AppM@. Class-method dispatch cannot be resolved without
  -- type information, so we surface the instance head and let consumers
  -- decide how to link class methods to their implementations.
  }

data Edge = Edge
  { fromName :: Text
  , fromMod :: Text
  , toName :: Text
  , toMod :: Text
  , kind :: EdgeKind
  , ambiguous :: Bool
  -- ^ The reference resolved to more than one candidate; every candidate is
  -- emitted so callers can over-approximate deliberately rather than by
  -- accident.
  }

data EdgeKind = CallEdge | TypeEdge
  deriving (Eq)

renderKind :: EdgeKind -> Text
renderKind CallEdge = "call"
renderKind TypeEdge = "type"

runCallGraph :: CallGraphOptions -> IO ()
runCallGraph opts = do
  config <- loadArboristConfig (Just opts.configFile)
  modFileMap <- buildModuleFileMap (allSourceRoots config)

  targets <- resolveTargets opts
  case targets of
    [] -> fail "No source files given: pass files as arguments or use --files-from"
    _ -> pure ()

  let total = length targets
  progress opts $ "Parsing " <> tshow total <> " files..."

  -- Phase 1: parse every target plus its transitive imports, visiting each
  -- module exactly once.
  (prgIndex, parsed) <- loadAll opts modFileMap targets

  -- Phase 2: resolve each module's exports once. This is what makes a batch
  -- viable: 'getExportedDecls' memoises re-export chains into the ExportIndex,
  -- but 'renamePrg' does not hand the updated cache back, so a per-file loop
  -- passing 'Map.empty' re-resolves the full export closure of every import for
  -- every file. Warming it here makes that work O(modules) instead of
  -- O(files x closure).
  progress opts $ "Resolving exports for " <> tshow (Map.size prgIndex) <> " modules..."
  let exportIndex = warmExportIndex prgIndex

  progress opts "Renaming and extracting..."
  let results = analyzeParsed prgIndex exportIndex <$> reverse parsed
      (defs, edges) = mconcat results
      keptEdges
        | opts.includeTypes = edges
        | otherwise = filter ((== CallEdge) . (.kind)) edges

  progress opts $
    "Done: " <> tshow (length defs) <> " definitions, " <> tshow (length keptEdges) <> " edges"

  emit opts (encodeGraph defs keptEdges)

-- | Resolve the exports of every indexed module, threading the memo cache.
warmExportIndex :: ProgramIndex -> ExportIndex
warmExportIndex prgIndex =
  List.foldl'
    (\idx modName -> snd (getExportedDecls prgIndex idx modName))
    Map.empty
    (Map.keys prgIndex)

-- | Parse the targets and everything they transitively import.
--
-- A breadth-first sweep with a shared visited set, rather than
-- 'gatherScopeDeps' per target. That function re-walks a file's whole import
-- closure on every call even when each module is already cached, and in this
-- repo almost any module's closure is ~10k modules -- so per-file calls cost
-- O(files x closure) where this costs O(modules).
loadAll ::
  CallGraphOptions ->
  Map.HashMap Hir.ModuleText FilePath ->
  [FilePath] ->
  IO (ProgramIndex, [(Hir.ModuleText, Hir.Read.Program)])
loadAll opts modFileMap targets = do
  -- Targets are analyzed; their imports are only indexed for name resolution.
  (index, analyzed) <- foldM readTarget (Map.empty, []) targets
  index' <- sweep index (concatMap (imports . snd) analyzed)
  pure (index', reverse analyzed)
 where
  imports prg = (.mod) <$> Hir.getImports prg

  readTarget (index, analyzed) path = do
    exists <- Dir.doesFileExist path
    if not exists
      then do
        hPutStrLn stderr $ "warning: skipping missing file " <> path
        pure (index, analyzed)
      else do
        prg <- parseFile path
        case prg.mod of
          Nothing -> do
            hPutStrLn stderr $ "warning: no module header in " <> path
            pure (index, analyzed)
          Just thisMod ->
            pure (Map.insert thisMod prg index, (thisMod, prg) : analyzed)

  sweep index [] = pure index
  sweep index (modName : rest)
    | Map.member modName index = sweep index rest
    | otherwise = case Map.lookup modName modFileMap of
        -- Not in any source root: a package dependency we cannot resolve.
        Nothing -> sweep index rest
        Just path -> do
          prg <- parseFile path
          when (Map.size index `mod` 2000 == 0) $
            progress opts $
              "  indexed " <> tshow (Map.size index) <> " modules..."
          sweep (Map.insert modName prg index) (imports prg <> rest)

  parseFile path = do
    contents <- TE.decodeUtf8 <$> BS.readFile path
    pure (snd (parsePrg contents))

-- | Rename one already-parsed program and extract its definitions and edges.
analyzeParsed ::
  ProgramIndex ->
  ExportIndex ->
  (Hir.ModuleText, Hir.Read.Program) ->
  ([Def], [Edge])
analyzeParsed prgIndex exportIndex (thisMod, prg) =
  case renamePrg prgIndex exportIndex prg of
    Nothing -> ([], [])
    Just renamed -> walk thisMod.text Nothing Nothing (AST.getDynNode renamed)

-- | Walk the renamed tree, tracking the enclosing binding and instance head.
--
-- Only the /outermost/ binding on a path becomes the attribution target, so
-- references inside @where@ and @let@ helpers collapse into the top-level
-- definition that owns them. Those helpers are not callable from other modules,
-- so collapsing them keeps the graph smaller without losing any reachability.
walk :: Text -> Maybe Text -> Maybe Text -> AST.DynNode -> ([Def], [Edge])
walk thisMod mInstance mCurrent node =
  (newDefs <> childDefs, hereEdges <> childEdges)
 where
  nodeTy = AST.nodeType node

  mInstance'
    | nodeTy == "instance" = instanceHeadOf node <|> mInstance
    | otherwise = mInstance

  mBound
    | isBinder nodeTy, isNothing mCurrent = binderName node
    | otherwise = Nothing

  mCurrent' = mCurrent <|> mBound

  newDefs = case mBound of
    Nothing -> []
    Just boundName ->
      [ Def
          { name = boundName
          , mod = thisMod
          , line = startLine node
          , instanceHead = mInstance'
          }
      ]

  -- Self-edges come from the binder's own name occurrence and from genuine
  -- recursion; neither adds anything to a reachability query.
  hereEdges = case mCurrent' of
    Nothing -> []
    Just caller -> filter (not . isSelfEdge) (edgesAt thisMod caller node)

  isSelfEdge e = e.toName == e.fromName && e.toMod == e.fromMod

  (childDefs, childEdges) =
    mconcat (walk thisMod mInstance' mCurrent' <$> node.nodeChildren)

isBinder :: Text -> Bool
isBinder ty = ty == "function" || ty == "bind"

-- | The bound name of a @function@ / @bind@ node: its leftmost variable child.
binderName :: AST.DynNode -> Maybe Text
binderName node =
  case mapMaybe varText node.nodeChildren of
    boundName : _ -> Just boundName
    [] -> Nothing
 where
  varText child
    | AST.nodeType child == "variable" = Just child.nodeText
    | otherwise = Nothing

-- | The instance head, e.g. @HasSubmitFee AppM@ for
-- @instance HasSubmitFee AppM where ...@.
instanceHeadOf :: AST.DynNode -> Maybe Text
instanceHeadOf node =
  case T.strip . fst . T.breakOn "where" . head' . T.lines $ node.nodeText of
    headText
      | T.null headText -> Nothing
      | otherwise -> Just (T.strip (dropKeyword headText))
 where
  head' ls = case ls of
    l : _ -> l
    [] -> ""
  dropKeyword t = fromMaybe t (T.stripPrefix "instance" t)

startLine :: AST.DynNode -> Int
startLine node = node.nodeLineColRange.start.line.pos

-- | Node types carrying resolution results we care about.
type ResolvedExt =
  Variable RenamePhase
    AST.:+ Constructor RenamePhase
    AST.:+ Name RenamePhase
    AST.:+ AST.Nil

edgesAt :: Text -> Text -> AST.DynNode -> [Edge]
edgesAt thisMod caller node =
  case AST.cast @ResolvedExt node of
    Just (AST.Inj @(Variable RenamePhase) varNode) ->
      case varNode.ext of
        Nothing -> []
        Just (ResolvedVariable (ResolvedGlobal glbl)) -> [varEdge False glbl]
        Just (AmbiguousGlobalVar glbls) -> varEdge True <$> NE.toList glbls
        -- Locals, fields and unresolved names carry no cross-module meaning.
        Just (ResolvedVariable (ResolvedLocal _)) -> []
        Just (AmbiguousLocalVar _) -> []
        Just ResolvedField -> []
        Just NoVarFound -> []
    Just (AST.Inj @(Constructor RenamePhase) conNode) ->
      case conNode.ext of
        Nothing -> []
        Just (ResolvedConstructor con) -> [conEdge False con]
        Just (AmbiguousConstructor cons) -> conEdge True <$> NE.toList cons
        Just NoConstructorFound -> []
    Just (AST.Inj @(Name RenamePhase) nameNode) ->
      case nameNode.ext of
        Nothing -> []
        Just (ResolvedName info _) -> [nameEdge False info]
        Just (AmbiguousName infos) -> nameEdge True <$> NE.toList infos
        Just NoNameFound -> []
    Just _ -> []
    Nothing -> []
 where
  mkEdge edgeKind isAmbiguous toName' toMod' =
    Edge
      { fromName = caller
      , fromMod = thisMod
      , toName = toName'
      , toMod = toMod'
      , kind = edgeKind
      , ambiguous = isAmbiguous
      }
  varEdge isAmbiguous glbl =
    mkEdge CallEdge isAmbiguous glbl.name.nameText glbl.originatingMod.text
  conEdge isAmbiguous con =
    mkEdge CallEdge isAmbiguous con.name.nameText con.originatingMod.text
  nameEdge isAmbiguous info =
    mkEdge TypeEdge isAmbiguous info.name.nameText info.originatingMod.text

resolveTargets :: CallGraphOptions -> IO [FilePath]
resolveTargets opts = do
  fromFile <- case opts.filesFrom of
    Nothing -> pure []
    Just listPath -> do
      contents <- TE.decodeUtf8 <$> BS.readFile listPath
      pure $ map T.unpack $ filter (not . T.null) $ map T.strip $ T.lines contents
  traverse Dir.makeAbsolute (List.nub (opts.sourceFiles <> fromFile))

encodeGraph :: [Def] -> [Edge] -> Value
encodeGraph defs edges =
  object
    [ "modules" .= List.nub (map (.mod) defs)
    , "functions" .= map encodeDef defs
    , "edges" .= map encodeEdge edges
    ]
 where
  encodeDef d =
    object
      [ "name" .= d.name
      , "module" .= d.mod
      , "line" .= d.line
      , "instanceHead" .= d.instanceHead
      ]
  encodeEdge e =
    object
      [ "from" .= e.fromName
      , "fromModule" .= e.fromMod
      , "to" .= e.toName
      , "toModule" .= e.toMod
      , "kind" .= renderKind e.kind
      , "ambiguous" .= e.ambiguous
      ]

emit :: CallGraphOptions -> Value -> IO ()
emit opts value =
  case opts.outputFile of
    Nothing -> BL8.putStrLn encoded
    Just target -> do
      outputAbs <- Dir.makeAbsolute target
      Dir.createDirectoryIfMissing True (takeDirectory outputAbs)
      BL8.writeFile outputAbs encoded
 where
  encoded = Aeson.encode value

tshow :: (Show a) => a -> Text
tshow = T.pack . show

progress :: CallGraphOptions -> Text -> IO ()
progress opts msg
  | opts.quiet = pure ()
  | otherwise = hPutStrLn stderr (T.unpack msg)
