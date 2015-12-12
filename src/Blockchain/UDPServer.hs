{-# LANGUAGE FlexibleContexts #-}

module Blockchain.UDPServer (
      runEthUDPServer,
      connectMe,
      udpHandshakeServer
     ) where

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as NB

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Resource

import           Data.Time.Clock.POSIX
import           Data.Time.Clock
import qualified Data.ByteString as B
import qualified Data.Text as T

import           Blockchain.UDP
import           Blockchain.SHA
import           Blockchain.Data.RLP
import           Blockchain.Data.DataDefs
import           Blockchain.DB.SQLDB
import           Blockchain.ExtWord
import           Blockchain.ExtendedECDSA
import           Blockchain.ContextLite
import           Blockchain.P2PUtil
                      
import           Data.Maybe
--
--import           Prelude 
import qualified Network.Haskoin.Internals as H
import qualified Crypto.Hash.SHA3 as SHA3
import           Crypto.PubKey.ECC.DH


    
runEthUDPServer::ContextLite->PrivateNumber->S.Socket->IO ()
runEthUDPServer cxt myPriv socket = do
  _ <- runResourceT $ flip runStateT cxt $ udpHandshakeServer (fromMaybe (error "invalid private nubmer in runEthUDPServer") $ H.makePrvKey $ fromIntegral myPriv) socket
  return ()

connectMe :: Int 
          -> IO S.Socket
connectMe port = do
  (serveraddr:_) <- S.getAddrInfo
                      (Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
                      Nothing (Just (show port))
  sock <- S.socket (S.addrFamily serveraddr) S.Datagram S.defaultProtocol
  S.bindSocket sock (S.addrAddress serveraddr) >> return sock

udpHandshakeServer :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m, MonadIO m) 
                   => H.PrvKey 
                   -> S.Socket
                   -> m ()
udpHandshakeServer prv conn = do
   (msg,addr) <- liftIO $ NB.recvFrom conn 1280  -- liftIO unavoidable?

   let (packet, otherPubkey) = dataToPacket msg
   liftIO $ putStrLn $ "Message Received from " ++ show otherPubkey
   liftIO $ print $ dataToPacket msg
                 
   let ip = sockAddrToIP addr

   time <- liftIO $ round `fmap` getPOSIXTime

   let (theType', theRLP) = ndPacketToRLP $
                                (Pong (Endpoint "127.0.0.1" 30303 30303) 4 (time+50):: NodeDiscoveryPacket)
                                
       theData = B.unpack $ rlpSerialize theRLP
       SHA theMsgHash = hash $ B.pack $ (theType':theData)

   ExtendedSignature signature' yIsOdd' <- liftIO $ H.withSource H.devURandom $ ecdsaSign  prv theMsgHash

   let v' = if yIsOdd' then 1 else 0 
       r' = H.sigR signature'
       s' = H.sigS signature'
       theSignature = word256ToBytes (fromIntegral r') ++ word256ToBytes (fromIntegral s') ++ [v']
       theHash = B.unpack $ SHA3.hash 256 $ B.pack $ theSignature ++ [theType'] ++ theData
   
   curTime <- liftIO $ getCurrentTime
 
   let peer = PPeer {
              pPeerPubkey = hPubKeyToPubKey $ otherPubkey,
              pPeerIp = T.pack ip,
              pPeerPort = 30305, -- change 
              pPeerNumSessions = 0,
              pPeerLastTotalDifficulty = 0,
              pPeerLastMsg  = T.pack "msg",
              pPeerLastMsgTime = curTime,
              pPeerLastBestBlockHash = SHA 0,
              pPeerVersion = T.pack "61" -- fix
            }
 
   _ <- addPeer $ peer
   _ <- liftIO $ NB.sendTo conn ( B.pack $ theHash ++ theSignature ++ [theType'] ++ theData) addr
   
   udpHandshakeServer prv conn
   return () 
