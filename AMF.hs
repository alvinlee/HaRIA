{-# OPTIONS_GHC -fglasgow-exts #-}
module AMF
  ( ShortString (..)
  , Object (..)
  , KeyValMap
  , MixedArray (..)
  , Array (..)
  , LongString (..)
  , Unknown (..)
  , AMFData (..)
  , AMFDatas (..)
  ) where

import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CBS (unpack)
import qualified Data.Map as DM
import qualified Data.List as DL

import Control.Monad (liftM,liftM2,replicateM,forM_)
import Data.Int
import Prelude hiding (String)

import BinaryPlus
-- import Debug.Trace

--------------------------------------------------------------------------------
newtype ShortString = ShortString { bsVal :: BS.ByteString } deriving(Eq,Ord,Show)
instance Binary ShortString where
  get = liftM ShortString $
        (get :: Get Word16)  >>=
        (getLazyByteString . fromIntegral)
  put (ShortString val)= do
    put (fromIntegral (BS.length val) :: Word16)
    putLazyByteString val

--------------------------------------------------------------------------------
newtype Object = Object { objVal :: KeyValMap } deriving(Eq,Show)
instance Binary Object where
  get = liftM Object $ getKeyValMap DM.empty
  put = putKeyValMap . objVal

type KeyValMap = DM.Map ShortString AMFData
getKeyValMap :: KeyValMap -> Get KeyValMap
getKeyValMap kvMap = do
  key <- get :: Get ShortString
  if BS.null $ bsVal key
     then do
       val <- lookAhead (get :: Get Word8)
       if val == 0x09
          then skip 1 >> return kvMap
          else return $ error "null key"
     else do
       val <- get :: Get AMFData
       let curMap = DM.insert key val kvMap
       getKeyValMap curMap

putKeyValMap :: KeyValMap -> Put
putKeyValMap kvMap = do
  mapM_ (\(key,val) -> put key >> put val) (DM.toList kvMap)
  put (0x00 :: Word16)
  put (0x09 :: Word8)

--------------------------------------------------------------------------------
data MixedArray = MixedArray { maSize :: Word32
                             , maVal  :: Object
                             } deriving(Eq,Show)
instance Binary MixedArray where
  get = liftM2 MixedArray get get
  put (MixedArray size val) = put size >> put val
  
--------------------------------------------------------------------------------
newtype Array = Array { arrayVal :: [AMFData] } deriving(Eq,Show)
instance Binary Array where
  get = do
    size <- get :: Get Word32
    liftM Array $ replicateM (fromIntegral size) (get :: Get AMFData)
  put (Array array) = do
    put (fromIntegral $ DL.length array :: Word32)
    mapM_ put array
          
--------------------------------------------------------------------------------
newtype LongString = LongString { lbsVal :: BS.ByteString } deriving(Eq,Show)
instance Binary LongString where
  get = liftM LongString $
        (get :: Get Word32)  >>=
        (getLazyByteString . fromIntegral)
  put (LongString val)= do
    put (fromIntegral (BS.length val) :: Word32)
    putLazyByteString val
  
--------------------------------------------------------------------------------
data Unknown = Unknown { unknownType :: Word8
                       , unknownVal  :: BS.ByteString
                       } deriving(Eq,Show)
instance Binary Unknown where
  get = liftM2 Unknown (return 0xFF) getRemainingLazyByteString
  put = putLazyByteString . unknownVal

--------------------------------------------------------------------------------
data AMFData = AMFNumber Double64
             | AMFBoolean Bool
             | AMFShortString ShortString
             | AMFObject Object
             | AMFMovieClip Unknown
             | AMFNull
             | AMFUndefined
             | AMFReference Word16
             | AMFMixedArray MixedArray
             | AMFArray Array
             | AMFDate Word64 Int16
             | AMFLongString LongString
             | AMFUnsupported
             | AMFRecordSet ShortString Object
             | AMFXML LongString
             | AMFTypedObject ShortString Object
             | AMFAMF3Data Unknown
             | AMFUnknown Unknown
               deriving(Eq,Show)

-- data AMFData = forall a.(Show a,Binary a) => AMFData { amfVal :: a }
-- instance Show AMFData where
--   show (AMFData val) = show val

instance Binary AMFData where
  get = do
    dataType <- get :: Get Word8
    case dataType of
      0x00 -> liftM  AMFNumber      get
      0x01 -> liftM  AMFBoolean     get
      0x02 -> liftM  AMFShortString get
      0x03 -> liftM  AMFObject      get
      0x04 -> liftM  AMFMovieClip   get
      0x05 -> return AMFNull
      0x06 -> return AMFUndefined
      0x07 -> liftM  AMFReference   get
      0x08 -> liftM  AMFMixedArray  get
      -- 0x09 -> 
      0x0a -> liftM  AMFArray       get
      0x0b -> liftM2 AMFDate        get get
      0x0c -> liftM  AMFLongString  get
      0x0d -> return AMFUnsupported
      0x0e -> liftM2 AMFRecordSet   get get
      0x0f -> liftM  AMFXML         get
      0x10 -> liftM2 AMFTypedObject get get
      0x11 -> liftM  AMFAMF3Data    get
      _    -> do
          unknown <- get
          return $ AMFUnknown $ unknown {unknownType = dataType}

  put (AMFNumber      val) = put ( 0x00 :: Word8 ) >> put val
  put (AMFBoolean     val) = put ( 0x01 :: Word8 ) >> put val
  put (AMFShortString val) = put ( 0x02 :: Word8 ) >> put val
  put (AMFObject      val) = put ( 0x03 :: Word8 ) >> put val
  put (AMFMovieClip   val) = put ( 0x04 :: Word8 ) >> put val
  put (AMFNull           ) = put ( 0x05 :: Word8 )
  put (AMFUndefined      ) = put ( 0x06 :: Word8 )
  put (AMFReference   val) = put ( 0x07 :: Word8 ) >> put val
  put (AMFMixedArray  val) = put ( 0x08 :: Word8 ) >> put val
  put (AMFArray       val) = put ( 0x0a :: Word8 ) >> put val
  put (AMFDate msec offset) = put ( 0x0b :: Word8 ) >> put msec >> put offset
  put (AMFLongString  val) = put ( 0x0c :: Word8 ) >> put val
  put (AMFUnsupported    ) = put ( 0x0d :: Word8 )
  put (AMFRecordSet si obj) = put ( 0x0e :: Word8 ) >> put si >> put obj
  put (AMFXML         val) = put ( 0x0f :: Word8 ) >> put val
  put (AMFTypedObject typ obj) = put ( 0x10 :: Word8 ) >> put typ >> put obj
  put (AMFAMF3Data    val) = put ( 0x11 :: Word8 ) >> put val
  put (AMFUnknown val@(Unknown typ _)) =
    put typ >> put val
                 
--------------------------------------------------------------------------------
newtype AMFDatas = AMFDatas { amfsVal :: [AMFData] } deriving(Eq,Show)
instance Binary AMFDatas where
  get = liftM AMFDatas $ safeGetList get
  put = (mapM_ put) . amfsVal
