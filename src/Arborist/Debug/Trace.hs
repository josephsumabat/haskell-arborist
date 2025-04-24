module Arborist.Debug.Trace (
  traceShowPretty,
)
where

import Data.Text.Lazy qualified as Text
import Debug.Trace
import Text.Pretty.Simple

traceShowPretty :: (Show s) => s -> a -> a
traceShowPretty p v = trace (Text.unpack . pShowNoColor $ p) v
