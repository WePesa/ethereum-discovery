{-# LANGUAGE OverloadedStrings #-}

module Executable.EthereumDiscovery (
  ethereumDiscovery
  ) where

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Maybe
import qualified Data.Text as T
import qualified Network.Socket as S
import qualified Network.Haskoin.Internals as H
    
import Blockchain.ContextLite
import Blockchain.UDPServer

privateKey :: H.PrvKey
privateKey = fromMaybe (error "Bad value for hardcoded private key in Main.hs") $ H.makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

{-
listenPort::Int
listenPort = 30303
-}

type ListenPort = Int
           
ethereumDiscovery::ListenPort->[String]->LoggingT IO ()
ethereumDiscovery listenPort args = do
          
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
      (connectMe bootstrapAddr bootstrapPort privateKey listenPort)
      (liftIO . S.sClose)
      (runEthUDPServer cxt privateKey)


  return ()
