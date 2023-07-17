{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

import Cardano.Tracer.CNSA.CnsaSink

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "Usage: cnsa-sink (/path/to/local/sock)+"
  fs -> runCnsaSink fs
