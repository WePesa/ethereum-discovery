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
    
import qualified Blockchain.Colors as CL
import Blockchain.ContextLite
import Blockchain.Data.PubKey
import Blockchain.EthConf
import Blockchain.P2PUtil
import Blockchain.UDPServer

privateKey :: H.PrvKey
privateKey = fromMaybe (error "Bad value for hardcoded private key in Main.hs") $ H.makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

ethereumDiscovery::LoggingT IO ()
ethereumDiscovery = do
  logInfoN $ T.pack $ CL.blue "Welcome to ethereum-discovery"
  logInfoN $ T.pack $ CL.blue "============================="
  logInfoN $ T.pack $ CL.green " * My NodeID is " ++ show (B16.encode $ B.pack $ pointToBytes $ hPubKeyToPubKey $ H.derivePubKey privateKey)
  
  _ <- runResourceT $ do
    cxt <- initContextLite

    bracket
      (connectMe $ discoveryPort $ discoveryConfig ethConf)
      (liftIO . S.sClose)
      (runEthUDPServer cxt privateKey (discoveryPort $ discoveryConfig ethConf))


  return ()
