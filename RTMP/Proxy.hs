module RTMP.Proxy
  ( proxyEventHandle
  , module ServerHelper
  ) where

import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString as SB
import System.IO

import ServerHelper
import RTMP.Protocol.Helper

proxyEventHandle :: SockAddr -> SocketEventHandle
proxyEventHandle serverAddr tvar (sock,addr) = bracket
  (setSocketOption sock NoDelay 0 >> socketToHandle sock ReadWriteMode)
  (\h -> hClose h >> putStrLn "client handle closed!") $
  \hClient -> bracket
    (connectTcpServer serverAddr >>=
      \sockServer -> socketToHandle sockServer ReadWriteMode)
    (\h -> hClose h >> putStrLn "server handle closed!") -- hClose 
    (proxyStream tvar hClient)

-- looseConcat :: BS.ByteString -> BS.ByteString
-- looseConcat bs = ret where
--   chunks = BS.toChunks bs
--   ret = BS.fromChunks $ looseConcat' chunks
  
-- looseConcat' :: [SB.ByteString] -> [SB.ByteString]
-- looseConcat' (one:two:other) = (SB.append one two) : looseConcat' other

-- test = do
--   ret <- BS.getContents
--   print $ looseConcat ret

proxyStream :: ThreadVar -> Handle -> Handle -> IO ()
proxyStream tvar hClient hServer = do
  curTVar <- newThreadVar
  
  -- hSetBuffering hClient $ BlockBuffering $ Just 1024
  -- hSetBuffering hServer $ BlockBuffering $ Just 1024
  
  hSetBuffering hClient NoBuffering
  hSetBuffering hServer NoBuffering
  
  clientStream <- BS.hGetContents hClient
  serverStream <- BS.hGetContents hServer

  -- stream chain
  traceIO tvar $ traceThread curTVar $ BS.hPut hServer (idStream clientStream) `finally`
          (putStrLn "server put exit!" >> stopAllThread curTVar)
          
  traceIO tvar $ traceThread curTVar $ BS.hPut hClient (idStream serverStream) `finally`
          (putStrLn "client put exit!" >> stopAllThread curTVar)

  -- stream decode
  traceIO tvar $ traceThread curTVar $ watchClientStream (idStream clientStream) `finally`
          (putStrLn "client stream exit!" >> stopAllThread curTVar)
          
  traceIO tvar $ traceThread curTVar $ watchServerStream (idStream serverStream) `finally`
          (putStrLn "server stream exit!" >> stopAllThread curTVar)

  -- stream dump
  -- traceIO tvar $ traceThread curTVar $ BS.writeFile "/tmp/client1" (clientStream) `finally`
  --         (putStrLn "client dump exit!" >> stopAllThread curTVar)

  -- traceIO tvar $ traceThread curTVar $ BS.writeFile "/tmp/client2" (idStream clientStream) `finally`
  --         (putStrLn "client dump exit!" >> stopAllThread curTVar)

  -- flush timer        
  -- traceIO tvar $ traceThread curTVar $ flushHandle hClient `finally`
  --         (putStrLn "client flush exit!" >> stopAllThread curTVar)
          
  -- traceIO tvar $ traceThread curTVar $ flushHandle hServer `finally`
  --         (putStrLn "server flush exit!" >> stopAllThread curTVar)

  traceThread curTVar $ waitAllThread curTVar `finally` putStrLn "client handle exit!"

-- buffering and flush at time
-- flushHandle :: Handle -> IO ()
-- flushHandle h = loop where
--   loop = threadDelay 1000000 >> putStrLn "flush!" >> hFlush h >> loop
  
watchClientStream :: BS.ByteString -> IO ()
watchClientStream bs =
  dumpRTMPBS False "\t==>>> " bs
  -- BS.writeFile "/home/alvin/src/HaRIA/doc/dumpup" bs
          
watchServerStream :: BS.ByteString -> IO ()
watchServerStream bs =
  dumpRTMPBS False "\t<<<== " bs
  -- BS.writeFile "/home/alvin/src/HaRIA/doc/dumpdown" (idStream bs)
