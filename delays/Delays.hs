{-# LANGUAGE LambdaCase #-}

module Delays where

import Cardano.Tracer.CNSA.BlockState (averageHeaderDownloadDelays)
import Cardano.Tracer.CNSA.BlockState.DB (initializeBlockDB, readBlockData)
import qualified Data.Aeson as JSON
import qualified Data.Map as Map

main :: IO ()
main =
  initializeBlockDB "blocks" >>= \case
    Left err -> error err
    Right hdl ->
      do
        blockData <- readBlockData hdl
        JSON.encodeFile "block-data.json" (Map.fromList blockData)
        let delays = averageHeaderDownloadDelays [bd | (_hash, bd) <- blockData]
        JSON.encodeFile "delays.json" delays
