module Numeric.Optimization.Algorithms.CMAES (
       minimize, minimizeIO
)where

import Data.List (isPrefixOf)
import System.IO
import System.Process

import Paths_cmaes

wrapFn = "cmaes_wrap.py"
commHeader = "<CMAES_WRAP_PY2HS>"

recvLine :: Handle -> IO String
recvLine h = do
  str <- hGetLine h
  if commHeader `isPrefixOf` str
    then return $ unwords $ drop 1 $ words str
    else do
      recvLine h

sendLine :: Handle -> String -> IO ()
sendLine h str = do
  hPutStrLn h str
  hFlush h



minimize :: (Show a, Read a, Show b) => [a] -> ([a]-> b) -> IO [a]
minimize init f = minimizeIO init (return . f)

minimizeIO :: (Show a, Read a, Show b) => [a] -> ([a]-> IO b) -> IO [a]
minimizeIO initxs func = do
  fn <- getDataFileName wrapFn          
  (Just hin, Just hout, _, _) <- createProcess (proc "python2" [fn])
    { std_in = CreatePipe, std_out = CreatePipe }
  sendLine hin $ unwords (map show initxs)
  sendLine hin $ "1"
  sendLine hin $ "0"
  let loop hin hout = do
      str <- recvLine hout
      let ws = words str
      case ws!!0 of
        "a" -> do
          return $ map read $ drop 1 ws
        "q" -> do
          ans <- func $ map read $ drop 1 ws
          sendLine hin $ show ans
          loop hin hout
  loop hin hout


