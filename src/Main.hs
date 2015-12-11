{-# LANGUAGE OverloadedStrings          #-}



import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as S
    
import Blockchain.ContextLite
import Blockchain.UDPServer



connStr :: BC.ByteString
connStr = "host=localhost dbname=eth user=postgres password=api port=5432"


privateKey :: Integer
privateKey =  0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

listenPort::Int
listenPort = 30303
    
main = do
  putStrLn "Starting Discovery daemon"

  let myPriv = privateKey
  
  _ <- runResourceT $ do
         cxt <- initContextLite connStr
         
         liftIO $ S.withSocketsDo $ bracket (connectMe listenPort) S.sClose (runEthUDPServer cxt myPriv)


  return ()
