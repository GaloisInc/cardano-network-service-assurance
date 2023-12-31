{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Cardano.Tracer.CNSA.Run.ParseLogs
 ( getLogBody
 , getPeerFromTraceObject
 , getFieldFromTraceObject
 , getFieldFromTraceObject'
 , LogBody(..)
 , EtcLogBody(..)
 , Addr
 , Peer
 , Sampler
 )
where

-- base:
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Network.HostName (HostName)
import           Text.Read (readMaybe)

-- package extra:
import           Data.Either.Extra (maybeToEither)

-- package text-short:
import qualified Data.Text.Short as TS

-- package cardano*...:
import           Cardano.Slotting.Slot
import qualified Cardano.Logging.Types as Log

-- package locli:
import           Cardano.Analysis.API.Ground ()
import           Cardano.Unlog.LogObject



---- Types ---------------------------------------------------------

type Addr    = String   -- Intepreted as Hostname:SocketNo
type Peer    = Addr     -- used 'semantically' for just the peer addresses
type Sampler = Addr -- Addresses which refer to the Sampler nodes


data LogBody =
    LB_LOBody LOBody -- capturing everything that locli unparses
  | LB_Etc    EtcLogBody
  deriving (Eq,Show)
  
data EtcLogBody =
  PeersFromNodeKernel [(HostName, Maybe SlotNo)]
  deriving (Eq,Show)


---- Functions -----------------------------------------------------

getFieldFromTraceObject
  :: [Key]               -- dive into the field
  -> (Value -> Parser a)
  -> Log.TraceObject
  -> Either String a
getFieldFromTraceObject keys parse' trObj =
  do
  x <- Log.toMachine trObj `maybeToEither'` "no 'toMachine' entry"
  flip decodeAndParse x $ \x'->
    do
    o <- projectField keys x'
    parse' o

getFieldFromTraceObject' 
  :: FromJSON a
  => [Key]
  -> Log.TraceObject
  -> Either String a
getFieldFromTraceObject' ks =
  getFieldFromTraceObject ks parseJSON
  
projectField :: [Key] -> Value -> Parser Value
projectField []     = return
projectField (k:ks) = withObject "Object" $ \o->
                         (o .: k) >>= projectField ks

getPeerFromTraceObject :: Log.TraceObject -> Either String Peer
getPeerFromTraceObject trObj =
  (\s-> case words s of {[_,p] -> p; _ -> error "getPeer"})
  <$>
  getFieldFromTraceObject' ["peer","connectionId"] trObj
  
getLogBody :: Log.TraceObject -> Either String LogBody
getLogBody trObj =
     fmap LB_LOBody (getLOBody     trObj)
  <> fmap LB_Etc    (getEtcLogBody trObj)

getEtcLogBody :: Log.TraceObject -> Either String EtcLogBody
getEtcLogBody trObj =
  case ns of
    ["Net","Peers","List","PeersFromNodeKernel"] ->
        do
        x <- Log.toMachine trObj `maybeToEither'` "no 'toMachine' entry"
        decodeAndParse pPeersFromNodeKernel x
    _ ->  
        Left $ "unknown Log.TraceObject: " ++ show ns

  where
  ns = Log.toNamespace trObj 
    
            
pPeersFromNodeKernel :: Object -> Parser EtcLogBody
pPeersFromNodeKernel o =
  do
  peers <- o .: "peers"
  xs <- mapM pPeer2 peers
  return $ PeersFromNodeKernel xs

  where
  pPeer2 v' =
    do
    addr       <- v' .: "peerAddress"
    slotString <- v' .: "peerSlotNo"
    let mslot = SlotNo <$> readMaybe slotString
    return (addr, mslot)
    

---- utility code --------------------------------------------------

maybeToEither' :: Maybe b -> a -> Either a b
maybeToEither' = flip maybeToEither

decodeAndParse
  :: FromJSON a => (a -> Parser b) -> Text.Text -> Either String b
decodeAndParse p s =
  case decode (textToLazyBS s) of
    Just v ->
        case parseEither p v of
              Right x  -> Right x
              Left  s' -> Left $ "error parsing body: " ++ s'
    _               -> Left "cannot parse into json" 
                
  where    
  textToLazyBS = BL.fromChunks . return . Text.encodeUtf8 


---- use Locli to parse --------------------------------------------

getLOBody :: Log.TraceObject -> Either String LOBody
getLOBody trObj =
  case Log.toMachine trObj of
    Nothing  -> Left "no 'toMachine' object"
    Just txt -> parseBody ns txt
  where
  ns = Text.intercalate "." (Log.toNamespace trObj)

parseBody :: Text.Text -> Text.Text -> Either String LOBody
parseBody ns s =
  case Map.lookup (TS.fromText ns) interpreter of
    Nothing -> Left $ "unsupported namespace: " ++ Text.unpack ns
    Just p -> decodeAndParse p s

  where
  (_,_,interpreter) = interpreters


---- testing -------------------------------------------------------

{-
import           Cardano.Logging.Types
                   (TraceObject(..),DetailLevel(..),SeverityS(..))

t1 = parseBody "A.B" "{}"
t2 = parseBody "ChainSync.Client.DownloadedHeader" "{}"

t3 = decodeAndParse pPeer i3
i3 = "{\"block\":\"974289bd2dcccd313f4666bb338b280d56cfd77d2dd6e6e3f7ef965b0517cc20\",\"blockNo\":8917722,\"kind\":\"DownloadedHeader\",\"peer\":{\"connectionId\":\"192.168.61.191:40689 204.236.161.163:3001\"},\"slot\":95481420}"

t3 = makeLOBody trob01

trob01 = TraceObject {toHuman = Just "Peer is ConnectionId {localAddress = 192.168.61.191:40689, remoteAddress = 204.236.161.163:3001}. While following a candidate chain, we rolled forward by downloading a header. At (Block {blockPointSlot = SlotNo 95481420, blockPointHash = 974289bd2dcccd313f4666bb338b280d56cfd77d2dd6e6e3f7ef965b0517cc20})", toMachine = Just "{\"block\":\"974289bd2dcccd313f4666bb338b280d56cfd77d2dd6e6e3f7ef965b0517cc20\",\"blockNo\":8917722,\"kind\":\"DownloadedHeader\",\"peer\":{\"connectionId\":\"192.168.61.191:40689 204.236.161.163:3001\"},\"slot\":95481420}", toNamespace = ["ChainSync","Client","DownloadedHeader"], toSeverity = Info, toDetails = DNormal, toTimestamp = stub, toHostname = "tullsen-ubuntu20-b", toThreadId = "ThreadId 299"}

-}
