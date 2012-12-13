module FLV.Protocol
  ( Header (..)
  , TagInfo (..)
  , Audio (..)
  , Video (..)
  , Script (..)
  , Tag (..)
  , FLV (..)
  ) where

import Control.Monad (when,liftM,liftM2,liftM4,liftM5)
import qualified Data.ByteString.Lazy as BS

import AMF
import BinaryPlus

-----------------------------------------------------------------------------
data Header = Hdr { hSig1    :: Word8
                  , hSig2    :: Word8
                  , hSig3    :: Word8
                  , hVersion :: Word8
                  , hAudio   :: Bool
                  , hVideo   :: Bool
                  } deriving(Eq,Show)

headerSize :: Word32
headerSize = 9

instance Binary Header where
  get = do
    header <- liftM4 Hdr get get get get
    flags  <- get :: Get Word8
    offset <- get :: Get Word32
    let hasAudio = (flags .&. 0x04) > 0
        hasVideo = (flags .&. 0x01) > 0
        remaining = offset - headerSize
    when (remaining > 0) $ skip $ fromIntegral remaining
    return $ header hasAudio hasVideo

  put (Hdr sig1 sig2 sig3 ver hasAudio hasVideo) = do
    put sig1 >> put sig2 >> put sig3 >> put ver
    let flags = if hasAudio then 0x04 else 0x00 .|.
                if hasVideo then 0x01 else 0x00
    put (flags :: Word8) >> put headerSize

-----------------------------------------------------------------------------
data TagInfo = TagInf { tType     :: Word8
                      , tSize     :: Word24
                      , tStamp    :: Word24
                      , tStampExt :: Word8
                      , tStreamId :: Word24
                      } deriving(Eq,Show)

tagInfoSize :: Word32
tagInfoSize = 11

instance Binary TagInfo where
  get = liftM5 TagInf get get get get get
  put (TagInf typ size stamp stmpExt strm) = 
    put typ >> put size >> put stamp >>
    put stmpExt >> put strm

-----------------------------------------------------------------------------
data Audio = Audio { aFormat :: Word8
                   , aRate   :: Word8
                   , aBit16  :: Bool
                   , aStereo :: Bool
                   , aData   :: BS.ByteString
                   } deriving(Eq,Show)

audioInfoSize :: Word32
audioInfoSize = 1

instance Binary Audio where
  get = do
    flags <- get :: Get Word8
    bs <- getRemainingLazyByteString
    let format = shiftR (flags .&. 0xF0) 4
        rate   = shiftR (flags .&. 0x0C) 2
        bit    = (flags .&. 0x02 ) == 0x02
        stereo = (flags .&. 0x01 ) == 0x01
    return $ Audio format rate bit stereo bs
  put (Audio format rate bit stereo bs) = 
    let flags = shiftL (format .&. 0x0F) 4 .|.
                shiftL (format .&. 0x03) 2 .|.
                if bit then 0x02 else 0x00 .|.
                if stereo then 0x01 else 0x00
    in do
      put (flags :: Word8)
      putLazyByteString bs

-----------------------------------------------------------------------------
data Video = Video { vFrame  :: Word8
                   , vCodec  :: Word8
                   , vData   :: BS.ByteString
                   } deriving(Eq,Show)

videoInfoSize :: Word32
videoInfoSize = 1

instance Binary Video where
  get = do
    flags <- get :: Get Word8
    bs <- getRemainingLazyByteString
    let frame = shiftR (flags .&. 0xF0) 4
        codec = flags .&. 0x0F
    return $ Video frame codec bs
  put (Video frame codec bs) =
    let flags = shiftL (frame .&. 0x0F) 4 .|.
                codec .&. 0x0F
    in do
      put (flags :: Word8)
      putLazyByteString bs

-----------------------------------------------------------------------------
newtype Script = Script AMFDatas deriving(Eq,Show) -- notify
  
instance Binary Script where
  get = liftM Script get
  put (Script ls) = put ls

-----------------------------------------------------------------------------
data Tag = AudioTag TagInfo Audio Word32
         | VideoTag TagInfo Video Word32
         | ScriptTag TagInfo Script Word32
           deriving(Eq)

instance Show Tag where
  show (AudioTag info audio last) =
    "\nAudioTag:" ++ show last ++
    "\n\t" ++ show info ++
    "\n\t" ++ show (audio{aData = BS.empty})
    
  show (VideoTag info video last) = 
    "\nVideoTag:" ++ show last ++
    "\n\t" ++ show info ++
    "\n\t" ++ show (video{vData = BS.empty})

  show (ScriptTag info script last) = 
    "\nScriptTag:" ++ show last ++
    "\n\t" ++ show info ++
    "\n\t" ++ show script

instance Binary Tag where
  get = do
    info     <- get :: Get TagInfo
    bs       <- getLazyByteString $ fromIntegral $ w24Val $ tSize info
    lastSize <- get :: Get Word32
    return $ case tType info of
               0x08 -> AudioTag info (runGet get bs) lastSize
               0x09 -> VideoTag info (runGet get bs) lastSize
               0x12 -> ScriptTag info (runGet get bs) lastSize
  put (AudioTag info audio _) =
    let size  = audioInfoSize + fromIntegral (BS.length $ aData audio)
        info' = info { tType = 0x08, tSize = Word24 size}
        total = tagInfoSize + size
    in put info' >> put audio >> put total
  put (VideoTag info video _) =
    let size  = videoInfoSize + fromIntegral (BS.length $ vData video)
        info' = info { tType = 0x09, tSize = Word24 size}
        total = tagInfoSize + size
    in put info' >> put video >> put total
  put (ScriptTag info scirpt _) = 
    let bs    = runPut $ put scirpt
        size  = fromIntegral $ BS.length bs
        info' = info { tType = 0x12, tSize = Word24 size}
        total = tagInfoSize + size
    in put info' >> putLazyByteString bs >> put total
                     
-----------------------------------------------------------------------------
data FLV = FLV { fHeader :: Header
               , fBody   :: [Tag]
               } deriving(Eq,Show)

instance Binary FLV where
  get = liftM2 FLV get $ skip 4 >> safeGetList get
  put (FLV header body) =
    put header >> put (0 :: Word32) >> mapM_ put body

-----------------------------------------------------------------------------
-- test = do
--   flv <- decodeFile "/usr/lib/red5/webapps/oflaDemo/streams/Transformers.flv" :: IO FLV
--   -- print flv
--   -- print $ (decode :: BS.ByteString -> FLV) $ encode flv
--   -- encodeFile "/tmp/Transformers2.flv" $ (decode :: BS.ByteString -> FLV) $ encode flv
--   BS.writeFile "/tmp/Transformers2.flv" $ encode flv
