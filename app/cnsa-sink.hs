{-# LANGUAGE LambdaCase #-}

module Main(main) where

import System.Environment (getArgs)
import Cardano.Tracer.CNSA.Run.CnsaSink

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "Usage: cnsa-sink (/path/to/local/sock)+"
  fs -> runCnsaSink fs
