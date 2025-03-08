module Arborist.Scope where
import qualified Data.Text as T
import Hir.Types (Decl, ExportItem, ModuleText)
import qualified Hir.Types as Hir
import qualified Data.HashMap.Lazy as Map
import Hir
import Control.Error (fromMaybe)
import AST


data NameInfo =
  NameInfo {
    name :: T.Text
  , dynNode :: DynNode
  , originatingMod :: ModuleText
  , requiresQualifier :: Bool
  }

type AvailableNames = [NameInfo]

type ExportedNames = [NameInfo]

data Scope = Scope AvailableNames ExportedNames

type ImportedPrograms = Map.HashMap ModuleText Hir.Program

declToNameInfo :: ModuleText -> Decl -> NameInfo
declToNameInfo modName decl =
  NameInfo
    {
      name = (declName decl).node.nodeText
    , dynNode = declDynNode decl
    , originatingMod = modName
    , requiresQualifier = False
    }

getNamesFromImport :: ImportedPrograms -> Hir.Import -> AvailableNames
getNamesFromImport imports thisImport =
  undefined

getExportedNames :: Hir.Program -> [AvailableNames]
getExportedNames prg =
  undefined

getAvailableNames :: ImportedPrograms -> Hir.Program -> AvailableNames
getAvailableNames imports thisPrg = 
  let declaredNames = 
        case thisPrg.mod of
          Nothing -> []
          Just m -> declToNameInfo m <$> thisPrg.decls in
  declaredNames
  
  
