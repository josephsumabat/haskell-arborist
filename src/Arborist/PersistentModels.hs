module Arborist.PersistentModels (
  persistentModelFromDecl,
  requiredModelFiles,
  persistentModelImportModules,
) where

import AST (DynNode)
import AST qualified
import AST.Haskell qualified as H
import Control.Error (hush)
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.List qualified as List
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Hir (declDynNode)
import Hir.Types (Decl, HirRead, Import (..), ModuleText (..), Program (..))

-- | Extract the referenced model file from a declaration if it matches
-- the @mkModel $(discoverEntities) $(modelFile "foo")@ pattern.
persistentModelFromDecl :: Decl HirRead -> Maybe Text
persistentModelFromDecl decl =
  case collectModelFiles (declDynNode decl) of
    [] -> Nothing
    modelName : _ -> Just modelName

-- | Gather all model files referenced by mkModel splices in a program.
requiredModelFiles :: Program HirRead -> [Text]
requiredModelFiles program
  | not (importsPersistentModelHelpers program) = []
  | otherwise =
      List.nub
        ( declMatches
            <> collectModelFiles (AST.getDynNode program.node)
        )
 where
  declMatches = program.decls >>= maybeToList . persistentModelFromDecl

persistentModelImportModules :: [Text]
persistentModelImportModules = ["PersistentModels.Import"]

importsPersistentModelHelpers :: Program HirRead -> Bool
importsPersistentModelHelpers program =
  any matchesImport program.imports
 where
  matchesImport Import {mod} = moduleTextToText mod `elem` persistentModelImportModules

moduleTextToText :: ModuleText -> Text
moduleTextToText ModuleText {text} = text

collectModelFiles :: DynNode -> [Text]
collectModelFiles node =
  current <> (node.nodeChildren >>= collectModelFiles)
 where
  current =
    case AST.cast @H.TopSpliceP node of
      Just splice -> maybeToList (persistentModelNameFromSplice splice)
      Nothing -> []

persistentModelNameFromSplice :: H.TopSpliceP -> Maybe Text
persistentModelNameFromSplice splice = do
  guard (any hasModelApply modelFunctionNames)
  modelFileArg <- asum (findApply <$> modelFileFunctionNames)
  (AST.Inj @H.LiteralP modelFileLit) <- pure modelFileArg.getExpression
  modelFileLit <- hush (AST.unwrap modelFileLit)
  (AST.Inj @H.StringP modelFileStr) <- pure modelFileLit.children
  stripQuotes (modelFileStr.dynNode.nodeText)
 where
  hasModelApply name = isJust (findApply name)
  findApply name = AST.getDeepestSatisfying (matchApply name) splice.dynNode

  modelFunctionNames :: [Text]
  modelFunctionNames = ["mkModel", "mkModelWithSettings"]

  modelFileFunctionNames :: [Text]
  modelFileFunctionNames = ["modelFile", "mkModelFile", "testModelFile", "asaModelFile"]

  matchApply :: Text -> DynNode -> Maybe H.ExpressionP
  matchApply name node = do
    apply <- AST.cast @H.ApplyP node
    apply' <- hush (AST.unwrap apply)
    fun <- apply'.function
    (AST.Inj @H.ExpressionP funExpr) <- pure fun
    (AST.Inj @H.VariableP funVar) <- pure funExpr.getExpression
    guard (funVar.dynNode.nodeText == name)
    (AST.Inj @H.ExpressionP argExpr) <- pure apply'.argument
    pure argExpr

stripQuotes :: Text -> Maybe Text
stripQuotes text = do
  withoutPrefix <- T.stripPrefix "\"" text
  T.stripSuffix "\"" withoutPrefix
