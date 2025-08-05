module Arborist.TreeSitter (parseExpectedNode) where

import AST qualified
import Control.Applicative
import Data.Pos qualified as Pos
import Data.Text qualified as Text
import TreeSitter.Api qualified as TS
import TreeSitter.Haskell qualified as TS

data SingleNodeParseFailure
  = ExpectedNodeNotFound
  | RangeNotMatched AST.DynNode Text.Text
  deriving (Show, Eq)

-- | Helper to parse a single node of text Examples:
--  `parseSingleNode @VariableP "myVariable"` - will result in a valid VariableP node
--  `parseSingleNode @VariableP "import MyImport"` - will result in an error
--  `parseSingleNode @ImportP "import MyImport"` - will result in a ImportP node
-- the text must be the entire node and vice versa
parseExpectedNode :: forall a. (AST.HasDynNode a, AST.Cast a) => Text.Text -> Either SingleNodeParseFailure a
parseExpectedNode t =
  let satisfyingNode =
        getFirstSatisfying (AST.cast @a) (TS.parse TS.tree_sitter_haskell t)
   in case satisfyingNode of
        Nothing -> Left ExpectedNodeNotFound
        Just res ->
          let dynNode = AST.getDynNode res
              (start, end) = (dynNode.nodeRange.start, dynNode.nodeRange.end)
           in case (start == Pos.mkPos 0, end == (Pos.mkPos $ Text.length t)) of
                (True, True) -> Right res
                _ -> Left $ RangeNotMatched dynNode t

renderFailure :: SingleNodeParseFailure -> Text.Text
renderFailure s =
  case s of
    RangeNotMatched dynNode _ -> "Statement range must match the entire input. node had range " <> (Text.pack . show $ dynNode.nodeRange)
    ExpectedNodeNotFound -> "Failed to find expected node"

getFirstSatisfying :: (AST.DynNode -> Maybe b) -> AST.DynNode -> Maybe b
getFirstSatisfying f n = go n
 where
  go n =
    f n <|> asum (go <$> (TS.nodeChildren n))
