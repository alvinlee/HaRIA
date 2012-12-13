module RTMP.Protocol.Object.Client
  ( Client (..)
  ) where

data Client = Client { clientInfoVal :: AMF.KeyValMap
                     } deriving(Eq,Show)