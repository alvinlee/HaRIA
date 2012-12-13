module RTMP.Class
  (
  ) where

-- Server -- Application -- Client -- Stream -- SharedObject

-- Server
-- onMessage
-- onInvoke

class Client c where
  getClientInfo :: c -> AMF.KeyValMap

class Server s where
  deployApp    :: Application a => s -> a -> s
  removeApp    :: Application a => s -> a -> s
  acceptClient :: Client c => s -> c -> Maybe s

class Application a where
  getAppName :: a -> BS.ByteString
  acceptClient :: Client c => s -> c -> Maybe s
  
  