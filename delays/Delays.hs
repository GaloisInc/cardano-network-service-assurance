{-# LANGUAGE LambdaCase #-}

module Delays where

import Cardano.Tracer.CNSA.BlockState (averageHeaderDownloadDelays)
import Cardano.Tracer.CNSA.BlockState.DB (initializeBlockDB, readBlockData)
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import System.Environment (getArgs)

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [dbName] -> analyze dbName
      _ -> analyze "blocks" -- FIXME: this is hardcoded in `CnsaAnalyses`, should share

analyze :: String -> IO ()
analyze dbName =
  initializeBlockDB dbName >>= \case
    Left err -> error err
    Right hdl ->
      do
        blockData <- readBlockData hdl
        putStrLn $ "found " <> show (length blockData) <> " entries in " <> show dbName

        let delays = averageHeaderDownloadDelays [bd | (_hash, bd) <- blockData]

        writeJSON "block-data.json" (Map.fromList blockData)
        writeJSON "delays.json" delays
  where
    writeJSON file object =
      do
        putStrLn $ "writing to " <> file
        JSON.encodeFile file object
