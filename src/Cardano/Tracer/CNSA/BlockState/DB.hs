{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.CNSA.BlockState.DB
  ( BlockDBHandle,
    initializeBlockDB,
    readBlockData,
    writeBlockData,
  )
where

import Cardano.Analysis.API.Ground (Hash (..))
import Cardano.Tracer.CNSA.BlockState
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Short as TS
import Data.Time
import qualified Data.Vector as V
import Database.InfluxDB
  ( Database,
    Field (FieldString),
    Line (..),
    Measurement,
    QueryResults (parseMeasurement),
    formatDatabase,
    formatQuery,
    getField,
    manage,
    parseJSON,
    query,
    queryParams,
    write,
    writeParams,
    (%),
  )
import qualified Database.InfluxDB.Format as F

newtype BlockDBHandle = Handle Database

-- | Create a database with the given name
initializeBlockDB :: String -> IO BlockDBHandle
initializeBlockDB dbStr =
  do
    let db = formatDatabase (fromString dbStr)
    manage (queryParams db) (formatQuery ("CREATE DATABASE " % F.database) db)
    pure (Handle db)

readBlockData :: BlockDBHandle -> IO [(Hash, BlockData)]
readBlockData (Handle db) = V.toList <$> query (queryParams db) selectAll
  where
    selectAll = formatQuery ("SELECT * FROM " % F.measurement) dbMeasure

writeBlockData :: BlockDBHandle -> (Hash, BlockData) -> IO ()
writeBlockData (Handle db) (hash, blockData) =
  let line = encodeLine hash blockData
   in write (writeParams db) line

--------------------------------------------------------------------------------

-- why doesn't this already exist?
instance (QueryResults a, QueryResults b) => QueryResults (a, b) where
  parseMeasurement precision name tags columns fields =
    (,)
      <$> parseMeasurement precision name tags columns fields
      <*> parseMeasurement precision name tags columns fields

instance QueryResults Hash where
  parseMeasurement _precision _name _tags columns fields =
    getField "hash" columns fields >>= parseJSON

instance QueryResults BlockData where
  -- In `encodeLine`, we encoded the `BlockData` as JSON and put it in the
  -- database as a string, so we have to undo both the stringification (by
  -- matching on `String`) and the JSON encoding (by using `eitherDecodeStrict`)
  parseMeasurement _precision _name _tags columns fields =
    do
      String text <- getField "blockData" columns fields
      case eitherDecodeStrict (encodeUtf8 text) of
        Right blockData -> pure blockData
        Left err -> fail err

--------------------------------------------------------------------------------

encodeLine :: Hash -> BlockData -> Line UTCTime
encodeLine (Hash hash) blockData = Line dbMeasure tags fields Nothing
  where
    tags = mempty
    fields =
      Map.fromList
        [ ("hash", FieldString (TS.toText hash)),
          ("blockData", FieldString (toStrict (encodeToLazyText blockData)))
        ]

dbMeasure :: Measurement
dbMeasure = "block-data"
