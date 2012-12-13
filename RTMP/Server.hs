module RTMP.Server
  ( serverEventHandle
  , module ServerHelper
  ) where

import qualified Data.ByteString.Lazy as BS
import Control.Monad(liftM2)
import System.IO

import ServerHelper
import BinaryPlus
import RTMP.Protocol.HandShake
import RTMP.Protocol.Message
import RTMP.Protocol.RTMP
import RTMP.Protocol.Helper


serverEventHandle :: SocketEventHandle
serverEventHandle tvar (sock,addr) = bracket
  (setSocketOption sock NoDelay 0 >> socketToHandle sock ReadWriteMode)
  (\h -> hClose h >> putStrLn "client handle closed!") $
  (serverStream tvar)

serverStream :: ThreadVar -> Handle -> IO ()
serverStream tvar hClient = do
  curTVar <- newThreadVar
  
  -- hSetBuffering hClient $ BlockBuffering $ Just 1024
  
  hSetBuffering hClient NoBuffering
  
  reqStream <- BS.hGetContents hClient

  let req = runGet (get :: Get RTMPData) reqStream

  resp <- handleRTMP req
  
  traceIO tvar $ traceThread curTVar $ dumpReq req `finally`
          (putStrLn "client req dump exit!" >> stopAllThread curTVar)
          
  traceIO tvar $ traceThread curTVar $ dumpResp resp `finally`
          (putStrLn "client resp dump exit!" >> stopAllThread curTVar)

  traceIO tvar $ traceThread curTVar $ responseStream hClient resp `finally`
          (putStrLn "client response exit!" >> stopAllThread curTVar)

  traceThread curTVar $ waitAllThread curTVar `finally` putStrLn "client handle exit!"

dumpReq :: RTMPData -> IO ()
dumpReq = dumpRTMP False "\t==>>> "

dumpResp :: RTMPData -> IO ()
dumpResp = dumpRTMP False "\t<<<== "

responseStream :: Handle -> RTMPData -> IO ()
responseStream hClient resp = BS.hPut hClient $ runPut $ put resp

handleRTMP :: RTMPData -> IO RTMPData
handleRTMP (RTMPData fstHndShk sndHndShk msgSeq) = do
  respMsg <- handleMsgs $ msgsVal msgSeq
  return $ RTMPData (HandShake $ BS.replicate 1536 0) fstHndShk $ Messages respMsg

handleMsgs :: [Message] -> IO [Message]
handleMsgs (cur:cont) = do
  liftM2 (:) (handleMsg cur) (handleMsgs cont)
handleMsgs [] = return []

handleMsg :: Message -> IO Message
handleMsg (Message hdr msg) = do
  -- case msg of
  --   Invoke amfs ->
  return $ Message hdr msg