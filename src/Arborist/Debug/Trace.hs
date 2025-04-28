module Arborist.Debug.Trace (
  traceShowPretty,
  traceShowMPretty,
  traceWhen,
  traceIdWhen,
  time,
)
where

import Data.Text.Lazy qualified as Text
import Data.Time
import Debug.Trace
import Text.Pretty.Simple

traceShowPretty :: (Show s) => s -> a -> a
traceShowPretty p v = trace (Text.unpack . pShowNoColor $ p) v

traceShowMPretty :: (Show a, Applicative f) => a -> f ()
traceShowMPretty p = traceM (Text.unpack . pShowNoColor $ p)

traceWhen :: (Show s) => Bool -> s -> a -> a
traceWhen cond p v =
  if cond then traceShowPretty p v else v

traceIdWhen :: (Show a) => Bool -> a -> a
traceIdWhen cond v =
  if cond then traceShowPretty v v else v

time :: [Char] -> IO a -> IO a
time label fn = do
  start <- getCurrentTime
  res <- fn
  end <- getCurrentTime
  putStrLn $ "Time to run " <> label <> ": " ++ show (diffUTCTime end start)
  pure res
