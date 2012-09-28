#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as T
import System.IO
import System.Process
import System.Posix.Unistd(usleep)

showT :: (Show a) => a -> T.ByteString
showT = T.pack . show

readT :: (Read a) => T.ByteString -> a
readT = read . T.unpack

ctrlStr :: T.ByteString
ctrlStr = "<CMAES_WRAP_PY2HS>"

recvLine :: Handle -> IO (Maybe T.ByteString)
recvLine h = do
  T.hPutStrLn stderr "begin recv"
  str <- T.hGetLine h
  T.hPutStrLn stderr $ showT str
  if ctrlStr `T.isPrefixOf` str
    then return $ Just $ T.unwords $ drop 1 $ T.words str
    else recvLine h

sendLine h str = do
  T.hPutStrLn stderr str
  T.hPutStrLn h str
  hFlush h

f :: [Double] -> Double
f xs = sum $ zipWith (\x i -> (x*x-i)**2) xs [1..]

probSize :: Int
probSize = 5

main = do
  (Just hin, Just hout, _, _) <- createProcess (proc "python2" ["./cmaes_wrap.py"])
    { std_in = CreatePipe, std_out = CreatePipe }
  usleep 1000000
  (Just str) <- recvLine hout
  T.putStr str
  sendLine hin $ T.unwords (replicate probSize "0")
  sendLine hin $ "1"
  sendLine hin $ "0"
  loop hin hout

loop hin hout = do
  T.hPutStrLn stderr "begin loop"
  (Just str) <- recvLine hout
  let ws = T.words str
  case ws!!0 of
    "a" -> do
      T.putStrLn str
    "q" -> do
      let ans = f $ map readT $ drop 1 ws
      sendLine hin $ showT ans
      loop hin hout
