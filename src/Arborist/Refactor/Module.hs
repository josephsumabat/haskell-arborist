module Arborist.Refactor.Module (
  renameModule,
  findImportingModules,
) where

import Arborist.ProgramIndex (ProgramIndex)
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
renameModule :: ProgramIndex -> Hir.ModuleText -> Hir.ModuleText -> SourceEdit
renameModule prgIndex oldMod newMod =
  let
    -- Compute an (unsafe) absolute path for a module's .hs file
    -- We coerce a relative path to AbsPath; the caller can normalize later
    pathForModule :: Hir.ModuleText -> Path.AbsPath
    pathForModule m =
      let rel = ModUtils.moduleToPath ".hs" m
       in Path.uncheckedCoercePath (Path.filePathToRel rel)

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
        else [ FsEditMoveFile { src = pathForModule oldMod, dst = pathForModule newMod } ]
  in
    SourceEdit { fileEdits = fileEditsMap, fsEdits = moveEdits }
