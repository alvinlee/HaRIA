module RTMP.Protocol.Protocol
  ( Header (..)
  , Protocol
  , Protocols (..)
  ) where

import Control.Monad
import Data.Int
import Data.Maybe
import Data.List as DL
import Data.IntMap as Map
import qualified Data.ByteString.Lazy as BS

import BinaryPlus

--------------------------------------------------------------------------------
-- headSize 2bit
-- channel  6bit
-- timer    3byte (headSize > 4)
-- bodySize 3byte (headSize > 8)
-- type     1byte (headSize > 8)
-- streamId 4byte (headSize > 12)
----------------
-- body
----------------
data Header = Header { hdrChanVal   :: Int
                     , hdrTimerVal  :: Word24
                     , hdrTotalVal  :: Word24
                     , hdrMsgVal    :: Word8
                     , hdrStrmVal   :: Word32
                     } deriving(Show,Eq)

--------------------------------------------------------------------------------
data Fragment = Fragment { frgHeaderVal    :: Header
                         , frgRemainingVal :: Word32
                         , frgIsNewVal     :: Bool
                         , frgBuffVal      :: BS.ByteString
                         } deriving(Show,Eq)
type LastFrags = IntMap (Header,Word32) -- last header and remaining size

getFragments :: LastFrags -> Get [(Fragment,LastFrags)]
getFragments lastFrags = do
  end <- isEmpty
  if end then return []
     else do
       cur@(_,curFrags) <- getFragment lastFrags
       cont <- getFragments curFrags
       return (cur : cont)

-- flag timer szMsg streamId
-- <---<---<---<---<---<---
--  3     2     1      0
getFragment :: LastFrags -> Get (Fragment,LastFrags)
getFragment lastFrags = do
  (flag, chan) <- getHdrChanFlags
  
  let mayLastFrag  = Map.lookup chan lastFrags :: Maybe (Header,Word32)
      hasLast      = isJust mayLastFrag
      (lastHdr,lastRemaining) = fromJust mayLastFrag
      lastFinish   = hasLast && lastRemaining == 0
      sameStreamId = flag > 0
      sameSzMsg    = flag > 1
      sameTimer    = flag > 2
      sameAll      = flag == 3
      isNew        = lastFinish || not sameAll || not hasLast

  timer <- if sameTimer then return $ hdrTimerVal lastHdr else get
  size  <- if sameSzMsg then return $ hdrTotalVal lastHdr else get
  msg   <- if sameSzMsg then return $ hdrMsgVal lastHdr   else get
  strm  <- if sameStreamId then return $ hdrStrmVal lastHdr else get

  let remain  = if isNew then w24Val size else lastRemaining
      curSize = if remain >= 128 then 128 else remain
      curHdr  = if sameAll then lastHdr else Header chan timer size msg strm
      curRemaining = remain - curSize
      curFrags     = Map.insert chan (curHdr,curRemaining) lastFrags
      
  body <- getLazyByteString (fromIntegral curSize)

  return (Fragment curHdr curRemaining isNew body,curFrags)

getHdrChanFlags :: Get (Word8,Int)
getHdrChanFlags = do
  fstByte <- get :: Get Word8
  
  -- 2 bit hdr size flg
  let hdrFlag = shiftR (fstByte .&. 0xc0) 6
      chnFlag = fstByte .&. 0x3f
      
  -- 6 bit + 0,1,2 byte,only 6 bit in hdr size ??
  chn <- case chnFlag of
           -- Two byte header
           0 -> return . (+64) . fromIntegral =<< (get :: Get Word8)
           -- Three byte header
           1 -> return . (+64) . fromIntegral =<< (get :: Get Word16)
           -- One Byte header
           _ -> return $ fromIntegral chnFlag
           
  return (hdrFlag,chn)

--------------------------------------------------------------------------------
type Protocol = (Header, BS.ByteString)

getProtos :: Get [Protocol]
getProtos = liftM (foldFragments . fst . DL.unzip) (getFragments Map.empty)

foldFragments :: [Fragment] -> [Protocol]
foldFragments (cur@(Fragment frgHeader frgRemaining frgIsNew frgBuff):cont)
  -- filter finished one
  | frgIsNew && (frgRemaining == 0) = (frgHeader,frgBuff) : foldFragments cont
      
  -- filter continue one
  | not frgIsNew = foldFragments cont

  -- fold cur channel continue one
  -- foldFragment wouldn't block ??'
  | otherwise = (foldFragment cur cont) : foldFragments cont
foldFragments [] = []

foldFragment :: Fragment -> [Fragment] -> Protocol
foldFragment (Fragment frgHeader frgRemaining frgIsNew frgBuff) cont =
  (frgHeader, foldedBuff) where
    foldedBuff    = BS.append frgBuff contBuff
    contBuff      = BS.concat contFrags
    contFrags     = takeContFragments (hdrChanVal frgHeader) cont

takeContFragments :: Int -> [Fragment] -> [BS.ByteString]
takeContFragments chan (cur@(Fragment frgHeader frgRemaining frgIsNew frgBuff):cont)
  -- filter other chan 
  | chan /= hdrChanVal frgHeader = takeContFragments chan cont

  -- error meet new one when not finished
  | frgIsNew = error "takeContProtos: unfinish proto"

  -- finished
  | frgRemaining == 0 = [frgBuff]

  -- continue
  | otherwise = frgBuff:takeContFragments chan cont
takeContFragments _ [] = []

type LastHeaders = IntMap Header

putProtos :: LastHeaders -> [Protocol] -> Put
putProtos lastHdrs (cur:cont) = do
  -- weave together with next ??
  let (puts,curHdrs) = putProtocol lastHdrs cur
  sequence_ puts
  putProtos curHdrs cont
putProtos _ [] = return ()

putProtocol :: LastHeaders -> Protocol -> ([Put],LastHeaders)
putProtocol lastHdrs (hdr,bs) = (curChunk:contChunks,curHdrs) where
  chan       = hdrChanVal hdr
  lastHdr    = Map.lookup chan lastHdrs :: Maybe Header
  curHdrs    = Map.insert chan hdr lastHdrs
  putHdr     = putHeader hdr lastHdr
  putContHdr = putHdrChanFlags 3 chan -- the same one
  cur:cont   = splitByLen 128 bs
  curChunk   = putHdr >> putLazyByteString cur
  contChunk  = \chunk -> putContHdr >> putLazyByteString chunk
  contChunks = DL.map contChunk cont

putHeader :: Header -> Maybe Header -> Put
putHeader (Header chan curTimer curSize curMsgNum curStreamId) mayLastHdr = 
  let 
    hasLast    = isJust mayLastHdr
    lastHdr    = fromJust mayLastHdr

    Header _ lastTimer lastSize lastMsgNum lastStreamId = lastHdr

    sameStrmId = hasLast && (curStreamId == lastStreamId)
    sameSzMsg  = sameStrmId && (curSize == lastSize) && (curMsgNum == lastMsgNum)
    sameTimer  = sameSzMsg && (curTimer == lastTimer)
    hdrFlag | sameTimer  = 3
            | sameSzMsg  = 2
            | sameStrmId = 1
            | otherwise  = 0
  in do
    putHdrChanFlags hdrFlag chan
    when (not sameTimer) $ put curTimer
    when (not sameSzMsg) $ put curSize
    when (not sameSzMsg) $ put curMsgNum
    when (not sameStrmId) $ put curStreamId
      
putHdrChanFlags :: Word8 -> Int -> Put
putHdrChanFlags hdr chan = do
  let hdrFlag = 0xc0 .&. shiftL hdr 6
      chanCur | chan < 64               = chan
              | chan < fromIntegral (maxBound::Word8)  = 0
              | chan < fromIntegral (maxBound::Word16) = 1
      chanCont = chan - 64
      chanFlag = 0x3f .&. (fromIntegral chanCont :: Word8)
      fstByte = hdrFlag .|. chanFlag

  put (fstByte :: Word8)

  case chanCur of
    0 -> put (fromIntegral chanCont :: Word8)
    1 -> put (fromIntegral chanCont :: Word16)
    _ -> return ()

splitByLen :: Int64 -> BS.ByteString -> [BS.ByteString]
splitByLen len bs =
  cur : if BS.null cont then []
        else (splitByLen len cont)
  where (cur,cont) = BS.splitAt len bs

--------------------------------------------------------------------------------
newtype Protocols = Protocols { protosVal :: [Protocol] } deriving(Show,Eq)
instance Binary Protocols where
  get = liftM Protocols getProtos
  put = putProtos Map.empty . protosVal
