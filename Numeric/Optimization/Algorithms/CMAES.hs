{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Numeric.Optimization.Algorithms.CMAES (
       run, Config(..),
       minimizer, minimizerIO, 
)where

import Control.Monad 
import Data.List (isPrefixOf)
import System.IO
import System.Process

import Paths_cmaes

data Config = Config
  { funcIO  :: [Double] -> IO Double
  , initXs  :: [Double] 
  , sigma0  :: Double
  , scaling :: Maybe [Double]
  }

defaultConfig :: Config
defaultConfig = Config
  { funcIO = error "funcIO undefined"
  , initXs = error "initXs undefined"
  , sigma0 = 0.25
  , scaling = Nothing
  }

minimizer :: [Double] -> ([Double]-> Double) -> Config
minimizer xs f = defaultConfig{ funcIO =  return . f, initXs = xs}

minimizerIO :: [Double] -> ([Double]-> IO Double) -> Config
minimizerIO xs fIO = defaultConfig{ funcIO = fIO, initXs = xs}

run :: Config -> IO [Double]
run Config{..} = do
  fn <- getDataFileName wrapperFn          
  (Just hin, Just hout, _, _) <- createProcess (proc "python2" [fn])
    { std_in = CreatePipe, std_out = CreatePipe }
  sendLine hin $ unwords (map show initXs)
  sendLine hin $ show sigma0
  sendLine hin $ show $ length options
  forM_ options $ \(key, val) -> do
    sendLine hin key
    sendLine hin val
  let loop = do
      str <- recvLine hout
      let ws = words str
      case ws!!0 of
        "a" -> do
          return $ map read $ drop 1 ws
        "q" -> do
          ans <- funcIO $ map read $ drop 1 ws
          sendLine hin $ show ans
          loop
        _ -> do
          fail "ohmy god        "
  loop
    where
      options :: [(String, String)]
      options = concat [kvpScaling]

      kvpScaling = case scaling of
        Nothing -> []
        Just xs -> [("scaling_of_variables", show xs)]


wrapperFn, commHeader :: String
wrapperFn = "cma_wrapper.py"
commHeader = "<CMA_WRAPPER_PY2HS>"

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

