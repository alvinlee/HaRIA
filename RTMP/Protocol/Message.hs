module RTMP.Protocol.Message
  ( MsgData (..)
  , Message (..)
  , Messages (..)
  ) where

import qualified Data.ByteString.Lazy as BS
import Control.Monad (liftM)
import Data.List as DL

import BinaryPlus
import qualified AMF hiding (Unknown)
import qualified RTMP.Protocol.SharedObject as SO
import qualified FLV.Protocol as FLV
import RTMP.Protocol.Protocol
import RTMP.Protocol.Ping
import RTMP.Protocol.Invoke

--------------------------------------------------------------------------------
data MsgData = ChunkSize Word32
             | BytesRead Word32
             | Ping PingData
             | ServerBW Word32
             | ClientBW Word32
             | AudioData FLV.Audio
             | VideoData FLV.Video
             | AMF3SharedObject
             | AMF3Message
             | Notify AMF.AMFDatas
             | SharedObject SO.SOData
             | Invoke InvokeData
             | Unknown Word8 BS.ByteString
               deriving(Show,Eq)

--------------------------------------------------------------------------------
data Message = Message { msgHeaderVal :: Header
                       , msgDataVal   :: MsgData
                       } deriving(Show,Eq)

decodeProtocol :: Protocol -> Message
decodeProtocol (hdr,buff) = Message hdr msg
  where msg = runGet getMsg buff
        getMsg = case hdrMsgVal hdr of
                   0x01 -> liftM ChunkSize get
                   -- 0x02 -> ??
                   0x03 -> liftM ChunkSize get -- always 4 byte ??
                   0x04 -> liftM Ping get -- 6 10 14 ??
                   0x05 -> liftM ServerBW get  -- always 4 byte ??
                   0x06 -> liftM ClientBW get  -- always 4 byte ?? + 1 ??
                   -- 0x07 -> ??
                   0x08 -> liftM AudioData get
                   0x09 -> liftM VideoData get
                   -- 0x0a-0xf -> ??
                   -- 0x10 -> AMF3SharedObject
                   -- 0x11 -> AMF3Message
                   0x12 -> liftM Notify get
                   0x13 -> liftM SharedObject get
                   0x14 -> liftM Invoke get
                   _    -> liftM (Unknown $ hdrMsgVal hdr) getRemainingLazyByteString

-- !!! need opt
encodeProtocol :: Message -> Protocol
encodeProtocol (Message hdr msgData) = (curHdr,buff) where
  curHdr = hdr {hdrMsgVal = msgNum, hdrTotalVal = Word24 $ fromIntegral $ BS.length buff}
  buff = runPut putMsg
  (msgNum,putMsg) = case msgData of
                      ChunkSize size   -> (0x01,put size)
                      --               -> 0x02 ??
                      BytesRead bytes  -> (0x03,put bytes)
                      
                      Ping ping        -> (0x04,put ping)
                      ServerBW bw      -> (0x05,put bw)
                      ClientBW bw      -> (0x06,put bw)
                      --               -> 0x07 ??
                      AudioData ad     -> (0x08,put ad)
                      VideoData vd     -> (0x09,put vd)
                      --               -> 0x0a-0xf ??
                      AMF3SharedObject -> (0x10,return ())
                      AMF3Message      -> (0x11,return ())
                      Notify amf       -> (0x12,put amf)
                      SharedObject so  -> (0x13,put so)
                      Invoke invk       -> (0x14,put invk)
                      Unknown code bs  -> (code,putLazyByteString bs)

--------------------------------------------------------------------------------
newtype Messages = Messages { msgsVal :: [Message] } deriving(Show,Eq)
instance Binary Messages where
  get = liftM decodeProtocols get
  put = put . encodeProtocols

decodeProtocols :: Protocols -> Messages
decodeProtocols = Messages . DL.map decodeProtocol . protosVal

encodeProtocols :: Messages -> Protocols
encodeProtocols = Protocols . DL.map encodeProtocol . msgsVal
