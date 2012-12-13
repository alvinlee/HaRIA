module RTMP.Protocol.Helper
  ( dumpRTMPBS
  , dumpRTMP
  , idStream
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Time
import Control.Monad (when)

import BinaryPlus
import RTMP.Protocol.HandShake
import RTMP.Protocol.Message
import RTMP.Protocol.RTMP
import FLV.Protocol

putCurTime = 
  getCurrentTime >>=
  utcToLocalZonedTime >>=
  putStr . show . localTimeOfDay . zonedTimeToLocalTime

dumpRTMPBS :: Bool -> String -> BS.ByteString -> IO ()
dumpRTMPBS time flags bs = dumpRTMP time flags $ runGet get bs

dumpRTMP :: Bool -> String -> RTMPData -> IO ()
dumpRTMP time flags (RTMPData fstHndShk sndHndShk msgs) =
  let put msg = when time putCurTime >> putStr flags >> dumpMsg msg
  in do
    when time putCurTime
    putStr flags
    putStr "First handshake:"
    putStrLn $ show $ BS.length $ hsVal fstHndShk
    -- BS.putStrLn $ hsVal fstHndShk

    when time putCurTime
    putStr flags
    putStr "Second handshake:"
    putStrLn $ show $ BS.length $ hsVal sndHndShk
    -- BS.putStrLn $ hsVal sndHndShk
  
    mapM_ put $ msgsVal msgs

dumpMsg :: Message -> IO ()
dumpMsg (Message _ (Unknown code bs)) =  BS.putStrLn (bs)
dumpMsg this@(Message _ (AudioData ad)) =
  putStrLn $ show this{msgDataVal = AudioData ad {aData = BS.empty}}
dumpMsg this@(Message _ (VideoData vd)) =
  putStrLn $ show this{msgDataVal = VideoData vd {vData = BS.empty}}
dumpMsg msg =  putStrLn (show msg)

idStream :: BS.ByteString -> BS.ByteString
idStream = runPut . put . runGet (get :: Get RTMPData)
