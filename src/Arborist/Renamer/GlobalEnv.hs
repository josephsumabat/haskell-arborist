module Arborist.Renamer.GlobalEnv where

import Hir.Types
import Data.Maybe
import TreeSitter.Api
import Data.HashMap.Lazy qualified as Map
import Data.Text
import Data.LineColRange
import qualified Hir.Types as Hir
import qualified Hir

data NameInfo = NameInfo
  {
    pos :: LineColRange
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

mkModIndex :: [Hir.Program] -> ModIndex
mkModIndex prgs =
  Map.fromList $ mapMaybe (\prg -> ((\mod -> (mod.text, mkNameIndex prg)) <$> prg.mod)) prgs

mkNameIndex :: Hir.Program -> NameIndex
mkNameIndex prg =
  Map.fromList $ (\decl -> (declNameText decl, declNameInfo decl)) <$> prg.decls
  where
    declNameInfo decl =
      NameInfo
        {
          pos = (Hir.declDynNode decl).nodeLineColRange
        , name = declNameText decl
        }
    declNameText decl = (Hir.declName decl).node.nodeText
