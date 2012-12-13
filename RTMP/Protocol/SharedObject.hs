module RTMP.Protocol.SharedObject
  ( SOData (..)
  ) where

import Control.Monad (liftM,liftM2,liftM4)
import qualified Data.ByteString.Lazy as BS

import BinaryPlus
import qualified AMF

--------------------------------------------------------------------------------
data Header = Header { nameVal    :: AMF.ShortString
                     , versionVal :: Word32
                     , persistVal :: Word32
                     , otherVal   :: Word32
                     } deriving(Show,Eq)
instance Binary Header where
  get = liftM4 Header get get get get
  put (Header name version persist other) =
    put name >> put version >> put persist >> put other

--------------------------------------------------------------------------------
data KeyValue = KeyValue { keyVal   :: AMF.ShortString
                         , valueVal :: AMF.AMFData
                         } deriving(Show,Eq)
instance Binary KeyValue where
  get = liftM2 KeyValue get get
  put (KeyValue key value) = put key >> put value

--------------------------------------------------------------------------------
data Command = Connect
             | Disconnect
             | SetReq KeyValue
             | SetNotify KeyValue
             | SetResp AMF.ShortString
             -- | SOServerSendMessage
             -- | SOClientStatus
             | ClearNotify
             -- | SOClientDeleteData
             -- | SOServerDeleteAttribute
             | InitNotify
             -- | SOClientDeleteAttribute
             -- | SOClientSendMessage
             | Unknown Word8 BS.ByteString
               deriving(Show,Eq)

instance Binary Command where
  get = do
    dataType <- get :: Get Word8
    dataSize <- get :: Get Word32
    case dataType of
      0x01 -> return Connect -- 0 body ??
      0x02 -> return Disconnect -- 0 body ??
      0x03 -> liftM  SetReq get
      0x04 -> liftM  SetNotify get
      0x05 -> liftM  SetResp get
      -- 0x06 -> ServerSendMessage
      -- 0x07 -> ClientStatus
      0x08 -> return ClearNotify -- 0 body ??
      -- 0x09 -> ClientDeleteData
      -- 0x0a -> ServerDeleteAttribute
      0x0b -> return InitNotify -- 0 body ??
      -- 0x?? -> SOClientDeleteAttribute
      -- 0x?? -> SOClientSendMessage
      _    -> liftM (Unknown dataType) (getLazyByteString $ fromIntegral dataSize)

  put Connect           = put ( 0x01 :: Word8 ) >> put ( 0x00 :: Word32 )
  put Disconnect        = put ( 0x02 :: Word8 ) >> put ( 0x00 :: Word32 )
  put (SetReq val)      = put ( 0x03 :: Word8 ) >> put size >> putLazyByteString bs where
                            bs = runPut $ put val
                            size = fromIntegral $ BS.length bs :: Word32
                          
  put (SetNotify val)   = put ( 0x04 :: Word8 ) >> put size >> putLazyByteString bs where
                            bs = runPut $ put val
                            size = fromIntegral $ BS.length bs :: Word32
                          
  put (SetResp val)     = put ( 0x05 :: Word8 ) >> put size >> putLazyByteString bs where
                            bs = runPut $ put val
                            size = fromIntegral $ BS.length bs :: Word32
                          
  put ClearNotify       = put ( 0x08 :: Word8 ) >> put ( 0x00 :: Word32 )
  put InitNotify        = put ( 0x0b :: Word8 ) >> put ( 0x00 :: Word32 )
  put (Unknown typ val) = put (  typ :: Word8 ) >> put size >> putLazyByteString bs where
                            bs = runPut $ put val
                            size = fromIntegral $ BS.length bs :: Word32

--------------------------------------------------------------------------------
data SOData = SOData { headerVal ::Header
                     , cmdsVal   :: [Command]
                     } deriving(Show,Eq)
instance Binary SOData where
  get = liftM2 SOData get $ safeGetList get
  put (SOData header cmds) =
    put header >> mapM_ put cmds
