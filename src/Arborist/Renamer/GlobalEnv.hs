module Arborist.Renamer.GlobalEnv where

import Data.HashMap.Lazy qualified as Map
import Data.LineColRange
import Data.Maybe
import Data.Text
import Hir qualified
import Hir.Types
import Hir.Types qualified as Hir
import TreeSitter.Api

data NameInfo = NameInfo
  { pos :: LineColRange
  , name :: Text
  }
  deriving (Show)

type ModIndex =
  Map.HashMap ModNameT NameIndex

type NameIndex =
  Map.HashMap NameT NameInfo

type ModNameT = Text

type NameT = Text

findName :: ModNameT -> NameT -> ModIndex -> Maybe NameInfo
findName modName name modIdx =
  Map.lookup modName modIdx >>= \nameIdx -> Map.lookup name nameIdx

emptyModIndex :: ModIndex
emptyModIndex = Map.empty

mkModIndex :: [Hir.Program hirKind] -> ModIndex
mkModIndex prgs =
  Map.fromList $ mapMaybe (\prg -> ((\mod -> (mod.text, mkNameIndex prg)) <$> prg.mod)) prgs

mkNameIndex :: Hir.Program hirKind -> NameIndex
mkNameIndex prg =
  Map.fromList $ (\decl -> (declNameText decl, declNameInfo decl)) <$> prg.decls
 where
  declNameInfo decl =
    NameInfo
      { pos = (Hir.declDynNode decl).nodeLineColRange
      , name = declNameText decl
      }
  declNameText decl = (Hir.declName decl).nameText
