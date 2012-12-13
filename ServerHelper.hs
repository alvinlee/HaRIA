module ServerHelper
  ( tcpServerStart
  , connectTcpServer
  , newThreadVar
  , traceIO
  , traceThread
  , stopAllThread
  , waitAllThread
  , ThreadVar
  , SocketEventHandle
  , module Network.Socket
  , module Control.Exception
  , module Control.Concurrent
  ) where

import qualified Data.IntMap as IM
import qualified Data.List as DL
import Control.Concurrent
import Control.Concurrent.MVar
import Network.Socket 
import Control.Exception (bracket,finally)
import Network.BSD (getProtocolNumber)
import Control.Monad (when)

type ThreadRecord = IM.IntMap (ThreadId,Bool)
type ThreadRecVar = MVar (Int,ThreadRecord)
type ThreadExitVar = MVar ()
type ThreadVar = (ThreadRecVar,ThreadExitVar)
type SocketEventHandle = ThreadVar -> (Socket, SockAddr) -> IO ()

--------------------------------------------------------------------
-- tcp server functions
tcpServerStart :: ThreadVar -> SockAddr -> SocketEventHandle -> IO ThreadId
tcpServerStart tvar addr hdl = traceIO tvar $
  bracket
      (tcpServerListen addr)
      (sClose)
      (tcpAcceptLoop tvar hdl)

tcpAcceptLoop :: ThreadVar -> SocketEventHandle -> Socket -> IO ()
tcpAcceptLoop tvar hdl serv = loop where
  loop = accept serv >>= 
    traceIO tvar . hdl tvar >> loop

tcpServerListen :: SockAddr -> IO Socket
tcpServerListen addr = do
  proto <- getProtocolNumber "tcp"
  sock <- socket AF_INET Stream proto
  setSocketOption sock ReuseAddr 1
  bindSocket sock addr
  listen sock maxListenQueue
  return sock
         
connectTcpServer :: SockAddr -> IO Socket
connectTcpServer addr = do
  proto <- getProtocolNumber "tcp"
  sock <- socket AF_INET Stream proto
  setSocketOption sock ReuseAddr 1
  setSocketOption sock NoDelay 0
  connect sock addr
  return sock
    
--------------------------------------------------------------------
-- thread manage functions
newThreadVar :: IO ThreadVar
newThreadVar = do
  recVar <- newMVar (0,IM.empty)
  exitVar <- newMVar ()
  return (recVar,exitVar)

traceIO :: ThreadVar -> IO () -> IO ThreadId
traceIO tvar run = forkIO $ traceThread tvar run

traceThread :: ThreadVar -> IO a -> IO a
traceThread (recVar,exitVar) run = bracket putCurId removeCurId (\idx -> run) where
  putCurId =
    modifyMVar recVar $ \(cur,thrdMap) -> do
      thrdId <- myThreadId

      -- block exit wait
      when (IM.null thrdMap) $ takeMVar exitVar
        
      let idx = genUniKey cur thrdMap
          idxNext = (idx+1)
          newMap = IM.insert idx (thrdId,False) thrdMap

      return ((idxNext,newMap),idx)

  removeCurId idx =
    modifyMVar_ recVar $ \(cur,thrdMap) -> do
      let newMap = IM.delete idx thrdMap

      -- unblock exit when last
      when (IM.null newMap) $ putMVar exitVar ()

      return (cur,newMap)

genUniKey :: Int -> ThreadRecord -> Int
genUniKey cur thrdMap =
  if IM.member cur thrdMap
  then genUniKey (cur+1) thrdMap
  else cur

stopAllThread :: ThreadVar -> IO ()
stopAllThread (recVar,_) =
  modifyMVar_ recVar $ \(cur,thrdMap) -> do
    thrdId <- myThreadId -- without this thread
    mapM_ killThread
            $ DL.filter (/= thrdId)
            $ DL.map (fst)
            $ DL.filter (not . snd) (IM.elems thrdMap)
            
    let newMap = IM.map (\(thrd,_) -> (thrd,True)) thrdMap
    return (cur,newMap)

waitAllThread :: ThreadVar -> IO ()
waitAllThread (recVar,exitVar) =
  withMVar exitVar $ \_ -> return ()
                
--------------------------------------------------------------------
-- test
-- eventHandle :: SocketEventHandle
-- eventHandle (sock,addr) =
--   EX.bracket
--       (socketToHandle sock ReadWriteMode)
--       (hClose)
--       (\h -> BS.hGetContents h >>= BS.putStrLn)

-- test :: IO ()
-- test = do
--   tvar <- newThreadVar
--   tcpServerStart tvar eventHandle (SockAddrInet 1936 iNADDR_ANY)

--   print =<< readMVar (fst tvar)
--   getChar
  
--   print =<< readMVar (fst tvar)
--   stopAllThread tvar
--   print =<< readMVar (fst tvar)
  
--   waitAllThread tvar
--   print =<< readMVar (fst tvar)
