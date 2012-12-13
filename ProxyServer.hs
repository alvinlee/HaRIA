module Main where

import Network.BSD
import Network.Socket
import System.IO
-- import Debug.Trace

import RTMP.Proxy
import RTMP.Server

import System.Posix.Signals

-- import System.Log.Logger
-- import System.Log.Handler.Syslog
-- import System.Log.Handler.Simple

-- loop = threadDelay 200000 >> putStrLn "flush!" >> loop

main :: IO ()
main = do
  -- loop
  -- h <- streamHandler DEBUG
  -- updateGlobalLogger "Server" (setLevel DEBUG)
  -- updateGlobalLogger "Server" (setHandlers [h])
                     
  -- warningM "Server" __FILE__  -- "Server Start" 

  installHandler openEndedPipe Ignore Nothing
  -- installHandler openEndedPipe (Catch (putStrLn "GOT SIGPIPE")) Nothing
                                
  tvar <- newThreadVar
  tcpServerStart tvar (SockAddrInet 1936 iNADDR_ANY) $
       proxyEventHandle (SockAddrInet 1935 iNADDR_ANY)

  tcpServerStart tvar (SockAddrInet 1937 iNADDR_ANY) serverEventHandle

  getChar
  stopAllThread tvar
  waitAllThread tvar