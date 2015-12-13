{-# LANGUAGE OverloadedStrings          #-}

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Maybe
import qualified Network.Socket as S
import qualified Network.Haskoin.Internals as H
    
import Blockchain.ContextLite
import Blockchain.UDPServer

privateKey :: H.PrvKey
privateKey = fromMaybe (error "Bad value for hardcoded private key in Main.hs") $ H.makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

listenPort::Int
listenPort = 30302
    
main::IO ()
main = do
  putStrLn "Starting Discovery daemon"

  _ <- runResourceT $ do
         cxt <- initContextLite

         liftIO $ S.withSocketsDo $ bracket (connectMe privateKey listenPort) S.sClose (runEthUDPServer cxt privateKey)


  return ()
