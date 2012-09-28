#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans
import Data.List (isPrefixOf)
import System.IO
import System.Process
import System.Posix.Unistd(usleep)


ctrlStr :: String
ctrlStr = "<CMAES_WRAP_PY2HS>"

recvLine :: Handle -> IO (Maybe String)
recvLine h = do
  str <- hGetLine h
  if ctrlStr `isPrefixOf` str
    then return $ Just $ unwords $ drop 1 $ words str
    else do
      hPutStrLn stderr str
      recvLine h

sendLine h str = do
  hPutStrLn h str
  hFlush h

f :: [Double] -> Double
f xs = sum $ zipWith (\x i -> (x*x-i)**2) xs [1..]

probSize :: Int
probSize = 5

main = do
  (Just hin, Just hout, _, _) <- createProcess (proc "python2" ["cmaes_wrap.py"])
    { std_in = CreatePipe, std_out = CreatePipe }
  sendLine hin $ unwords (replicate probSize "0")
  sendLine hin $ "1"
  sendLine hin $ "0"
  loop hin hout

loop hin hout = do
  (Just str) <- recvLine hout
  let ws = words str
  case ws!!0 of
    "a" -> do
      putStrLn str
    "q" -> do
      let ans = f $ map read $ drop 1 ws
      sendLine hin $ show ans
      loop hin hout
