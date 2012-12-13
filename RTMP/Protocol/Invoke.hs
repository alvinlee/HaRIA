module RTMP.Protocol.Invoke
  ( InvokeData (..)
  ) where
  
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CS


import Control.Monad (liftM2)
  
import qualified AMF
import BinaryPlus

--------------------------------------------------------------------------------
data InvokeType = Connect
                | Disconnect
                | CreateStream
                | DeleteStream
                | CloseStream
                | ReleaseStream
                | Publish
                | Pause
                | Seek
                | Play
                | ReceiveVideo
                | ReceiveAudio
                | Result
                | Error
                | Unknown BS.ByteString
                  deriving(Show,Eq)

decodeInvokeType :: AMF.AMFData -> InvokeType
decodeInvokeType (AMF.AMFShortString (AMF.ShortString method)) =
  case CS.unpack method of
    "connect"       -> Connect
    "disconnect"    -> Disconnect
    "createStream"  -> CreateStream
    "deleteStream"  -> DeleteStream
    "closeStream"   -> CloseStream
    "releaseStream" -> ReleaseStream
    "publish"       -> Publish
    "pause"         -> Pause
    "seek"          -> Seek
    "play"          -> Play
    "receiveVideo"  -> ReceiveVideo
    "receiveAudio"  -> ReceiveAudio
    "_result"       -> Result
    "_error"        -> Error
    _               -> Unknown method

encodeInvokeType :: InvokeType -> AMF.AMFData
encodeInvokeType invokeType = AMF.AMFShortString $ AMF.ShortString $
  case invokeType of
    Connect        -> CS.pack "connect" 
    Disconnect     -> CS.pack "disconnect"
    CreateStream   -> CS.pack "createStream"
    DeleteStream   -> CS.pack "deleteStream"
    CloseStream    -> CS.pack "closeStream"
    ReleaseStream  -> CS.pack "releaseStream"
    Publish        -> CS.pack "publish"
    Pause          -> CS.pack "pause"
    Seek           -> CS.pack "seek"
    Play           -> CS.pack "play"
    ReceiveVideo   -> CS.pack "receiveVideo"
    ReceiveAudio   -> CS.pack "receiveAudio"
    Result         -> CS.pack "_result"
    Error          -> CS.pack "_error"
    Unknown method -> method

--------------------------------------------------------------------------------
data InvokeData = InvokeData { invkMethodVal :: InvokeType
                             , invkIdVal     :: Double
                             , invkParmVal   :: AMF.AMFData
                             , invkParmsVal  :: AMF.AMFDatas
                             }deriving(Show,Eq)
instance Binary InvokeData where
  get = do
    invkMethod <- get
    (AMF.AMFNumber invkId) <- get
    let method = decodeInvokeType invkMethod
        id     = d64Val invkId
    liftM2 (InvokeData method id) get get

  put (InvokeData iwnvkMethod invkId invkParm invkParms) = do
    put $ encodeInvokeType iwnvkMethod
    put $ AMF.AMFNumber $ Double64 invkId
    put invkParm
    put invkParms
