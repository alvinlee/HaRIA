module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Concurrent
import Network.AcceptLoop
import Network.BSD
import Network.Socket
import System.IO
import Debug.Trace

-- import RTMP.Protocol

main = do
  sock <- open_socket
  shutdownCommand <- launchAcceptLoop onAccept sock
  putStrLn "Press Enter to shutdown gracefully: "
  getChar
  shutdownCommand
  putStrLn "Good bye."
  sClose sock

open_socket = do
  proto <- getProtocolNumber "tcp"
  sock <- socket AF_INET Stream proto
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 1936 iNADDR_ANY)
  listen sock maxListenQueue
  return sock

onAccept :: OnAccept
onAccept (clientSocket, clientAddress) = do
  putStrLn $ "onAccept:" ++ (show clientAddress)
  forkIO $ talkToClient clientSocket
  return ()

connectServer = do
  proto <- getProtocolNumber "tcp"
  sock <- socket AF_INET Stream proto
  setSocketOption sock ReuseAddr 1
  connect sock (SockAddrInet 1935 iNADDR_ANY)
  putStrLn "connected to server!"
  return sock

-- buffering and flush at time
-- waitFlush h = threadDelay 10000000 >> putStrLn "flush!" >> hFlush h >> waitFlush h

watchClientStream :: BS.ByteString -> IO ()
watchClientStream bs =
  dumpCmd "\n==>>> "
          bs $ getHandShake True >> getHandShake False
          
watchServerStream :: BS.ByteString -> IO ()
watchServerStream bs =
  dumpCmd "\n<<<== " bs $ getHandShake True >> getHandShake False

-- here may be not close when exit
talkToClient clientSocket = do
  svrSocket <- connectServer
  
  hcsock <- socketToHandle clientSocket ReadWriteMode
  hssock <- socketToHandle svrSocket ReadWriteMode

  -- hSetBuffering hcsock $ BlockBuffering $ Just 1024
  -- hSetBuffering hcsock LineBuffering
  
  hSetBuffering hcsock NoBuffering
  hSetBuffering hssock NoBuffering
  
  crecv <- BS.hGetContents hcsock
  srecv <- BS.hGetContents hssock

  forkIO $ watchClientStream crecv
  forkIO $ BS.hPut hssock crecv

  forkIO $ watchServerStream srecv
  forkIO $ BS.hPut hcsock srecv

  getContents
  
  return ()
  
  -- 1.connect to server
  -- 2.put stream in to server
  -- 3.get from server and return      
  
  -- h <- socketToHandle clientSocket ReadWriteMode
  -- get <- hGetContents h
  -- putStrLn get
  -- hPutStrLn h "Hello there."
  -- hClose h

  -- get <- recv clientSocket 1537
  -- appendFile "/home/alvin/flash/test" "\n-------------1\n"
  -- appendFile "/home/alvin/flash/test" get
  
  -- -- send response
  -- scount <- send clientSocket $ head get : (replicate 1536 'a') ++ (tail get)
  -- print scount

  -- -- 
  -- get2 <- recv clientSocket 1536
  -- appendFile "/home/alvin/flash/test" "\n-------------2\n"
  -- appendFile "/home/alvin/flash/test" get2

  -- -- last
  -- (get3,rcount) <- recvLen clientSocket 1500
  -- print rcount
  -- appendFile "/home/alvin/flash/test" "\n-------------3\n"
  -- appendFile "/home/alvin/flash/test" get3
  -- appendFile "/home/alvin/flash/test" "<-------------3\n"
  
  -- sClose clientSocket

-- import Control.Monad
-- httpEventHandle :: SocketEventHandle
-- httpEventHandle tvar (sock,addr) = bracket
--   (setSocketOption sock NoDelay 0 >> socketToHandle sock ReadWriteMode)
--   (hClose) httpLoop

-- httpLoop :: Handle -> IO ()
-- httpLoop hdl = loop where
--   loop = do
--     req <- hGetContents hdl
--     when  (httpReqLoop $ lines req)
--           (hPutStr hdl "HTTP/1.0 200 OK\r\nConnection: Keep-Alive\r\nContent-Length: 9\r\nContent-Type: text/plain\r\n\r\nIt Works!" >> return ())
--     loop

-- httpReqLoop :: [String] -> Bool
-- httpReqLoop (line:other) =
--   if length line == 1
--      then True
--      else httpReqLoop other
-- httpReqLoop _ = True
