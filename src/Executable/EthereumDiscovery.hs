{-# LANGUAGE OverloadedStrings #-}

module Executable.EthereumDiscovery (
  ethereumDiscovery
  ) where

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Maybe
import qualified Data.Text as T
import qualified Network.Socket as S
import qualified Network.Haskoin.Internals as H
    
import Blockchain.EthConf
import Blockchain.ContextLite
import Blockchain.Handshake
import Blockchain.P2PUtil
import Blockchain.UDPServer

privateKey :: H.PrvKey
privateKey = fromMaybe (error "Bad value for hardcoded private key in Main.hs") $ H.makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

ethereumDiscovery::[String]->LoggingT IO ()
ethereumDiscovery args = do
  logInfoN $ T.pack $ "pubkey " ++ (show $ B16.encode $ B.pack $ pointToBytes $ hPubKeyToPubKey $ H.derivePubKey privateKey)
  
  let (bootstrapAddr, bootstrapPort) =
       case args of
            [x, y] -> (x, y)
            [] -> ("52.16.188.185", "30303")
            _ -> error "params have wrong format"

  logInfoN $ T.pack $ "Bootstrap address: " ++ bootstrapAddr ++ ":" ++ bootstrapPort
            
  logInfoN "Starting Discovery daemon"

  _ <- runResourceT $ do
    cxt <- initContextLite

    bracket
      (connectMe bootstrapAddr bootstrapPort privateKey (discoveryPort $ discoveryConfig ethConf))
      (liftIO . S.sClose)
      (runEthUDPServer bootstrapAddr bootstrapPort cxt privateKey)


  return ()
