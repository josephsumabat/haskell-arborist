module Arborist.Refactor.Module (
  renameModule,
  findImportingModules,
) where

import Arborist.ProgramIndex (ProgramIndex)
import Arborist.Files (ModFileMap)
import Arborist.Rewrite (rewriteNode)
import AST qualified
import AST.Haskell qualified as H
import Data.Edit (Edit)
import Data.HashMap.Lazy qualified as Map
import Data.HashMap.Strict qualified as HashMap
import Data.SourceEdit (FsEdit (..), SourceEdit (..))
import Data.Path qualified as Path
import Data.Either.Extra (eitherToMaybe)
import Hir.Parse qualified as Hir.Parse
import Hir.Render.Import qualified as Render
import Hir.Read.Types qualified as Hir.Read
import Hir.Write.Types qualified as Hir.Write
import Hir.Types qualified as Hir
import ModUtils qualified as ModUtils
import System.FilePath qualified as FilePath
import Control.Applicative ((<|>))

-- | Find all modules in the program index that import the given module.
findImportingModules :: ProgramIndex -> Hir.ModuleText -> [Hir.ModuleText]
findImportingModules programIndex targetModule =
  Map.keys $ Map.filter hasImport programIndex
 where
  hasImport :: Hir.Program hirKind -> Bool
  hasImport program = any (\import_ -> import_.mod == targetModule) program.imports

-- | Create a SourceEdit that renames a module across the codebase:
--   - Rewrites the module header in the old module's file to the new name
--   - Rewrites all import statements that reference the old module
--   - Adds a filesystem edit to move the file to its new path
--
--   Namespace aliasing is preserved: if an import used an alias, the alias is kept.
renameModule :: ProgramIndex -> ModFileMap -> FilePath -> FilePath -> Hir.ModuleText -> Hir.ModuleText -> SourceEdit
renameModule prgIndex modFileMap oldBaseAbs newBaseAbs oldMod newMod =
  let
    -- Compute a best-effort absolute path for a module's .hs file
    -- Prefer entries from the provided ModFileMap; fall back to inferred path
    pathForModule :: Hir.ModuleText -> Path.AbsPath
    pathForModule m =
      let file = Map.lookup m modFileMap
                  <|> Map.lookup m fallbackMap
          absolutize p =
            let p' = if FilePath.isAbsolute p then p else FilePath.normalise (FilePath.combine oldBaseAbs p)
             in Path.unsafeFilePathToAbs p'
       in case file of
            Just fp -> absolutize fp
            Nothing -> absolutize (ModUtils.moduleToPath ".hs" m)

    -- Destination path under the new base for a module
    pathForModuleNewBase :: Hir.ModuleText -> Path.AbsPath
    pathForModuleNewBase m =
      let rel = ModUtils.moduleToPath ".hs" m
          absP = if FilePath.isAbsolute rel then rel else FilePath.normalise (FilePath.combine newBaseAbs rel)
       in Path.unsafeFilePathToAbs absP

    -- Fallback map using inferred relative paths for any modules not present
    fallbackMap =
      Map.fromList
        [ (m, ModUtils.moduleToPath ".hs" m)
        | (m, _prg) <- Map.toList prgIndex
        ]

    -- Rewrite a single import of the old module to the new module, preserving alias/flags
    rewriteImport :: Hir.Read.Import -> Edit
    rewriteImport imp =
      let writeImp :: Hir.Write.Import
          writeImp = Render.fromReadImport imp
          updated :: Hir.Write.Import
          updated =
            Hir.Import
              { Hir.mod = newMod
              , alias = writeImp.alias
              , qualified = writeImp.qualified
              , hiding = writeImp.hiding
              , importList = writeImp.importList
              , dynNode = ()
              }
          newText = Render.renderImport updated
       in rewriteNode imp.dynNode newText

    -- All import edits grouped by importing module
    importEditsByMod :: [(Hir.ModuleText, Edit)]
    importEditsByMod =
      Map.toList prgIndex >>= \(modName, prg) ->
        let edits = map rewriteImport (filter ((== oldMod) . (.mod)) prg.imports)
         in case edits of
              [] -> []
              es -> [(modName, mconcat es)]

    -- Header edit for the renamed module, if we have it parsed
    headerEdit :: Maybe Edit
    headerEdit = do
      prg <- Map.lookup oldMod prgIndex
      header <- Hir.Parse.findNode (AST.cast @H.HeaderP) (AST.getDynNode prg.node)
      headerU <- eitherToMaybe (AST.unwrap header)
      let moduleP = headerU.module'
      pure (rewriteNode (AST.getDynNode moduleP) newMod.text)

    -- Accumulate file edits keyed by file path
    fileEditsMap :: HashMap.HashMap Path.AbsPath Edit
    fileEditsMap =
      let base = HashMap.fromList [ (pathForModule m, e) | (m, e) <- importEditsByMod ]
          base' = case headerEdit of
                    Nothing -> base
                    Just e  -> HashMap.insertWith (<>) (pathForModule oldMod) e base
       in base'

    moveEdits :: [FsEdit]
    moveEdits =
      if oldMod == newMod
        then []
        else [ FsEditMoveFile { src = pathForModule oldMod, dst = pathForModuleNewBase newMod } ]
  in
    SourceEdit { fileEdits = fileEditsMap, fsEdits = moveEdits }
