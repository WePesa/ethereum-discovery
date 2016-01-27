{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Blockchain.UDPServer (
      runEthUDPServer,
      connectMe,
      udpHandshakeServer
     ) where

import Network.Socket
import qualified Network.Socket.ByteString as NB

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import Crypto.Types.PubKey.ECC
--import Crypto.Random
            
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Time.Clock.POSIX
import           Data.Time.Clock
import qualified Data.Text as T

import Data.Word
    
import           Blockchain.Format
import           Blockchain.UDP
import           Blockchain.SHA
import           Blockchain.Data.DataDefs
import           Blockchain.DB.SQLDB
import           Blockchain.ContextLite
import           Blockchain.P2PUtil
                      
import qualified Network.Haskoin.Internals as H
    
runEthUDPServer::ContextLite->H.PrvKey->Socket->IO ()
runEthUDPServer cxt myPriv sock = do
  _ <- runResourceT $ flip runStateT cxt $ udpHandshakeServer myPriv sock
  return ()

connectMe::String->String->H.PrvKey->Int 
          -> IO Socket
connectMe bootstrapAddr bootstrapPort prv port = do
--  (serveraddr:_) <- getAddrInfo
--                      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
--                      Nothing (Just (show port))
  (serveraddr:_) <- getAddrInfo
                      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                      Nothing (Just (show port))
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bindSocket sock (addrAddress serveraddr)

  time <- liftIO $ round `fmap` getPOSIXTime

--  let bootstrapAddr = "52.16.188.185"
--      bootstrapPort = "30303"
--  let bootstrapAddr = "poc-9.ethdev.com"
--      bootstrapPort = "30303"
--  let bootstrapAddr = "127.0.0.1"
--      bootstrapPort = "30303"
          
  (peeraddr:_) <- getAddrInfo Nothing (Just bootstrapAddr) (Just bootstrapPort)
--  sock2 <- socket (addrFamily peeraddr) Datagram defaultProtocol

  liftIO $ sendPacket sock prv (addrAddress peeraddr) $ Ping 4 (Endpoint (getHostAddress $ addrAddress serveraddr) 30303 30303) (Endpoint (getHostAddress $ addrAddress peeraddr) 30303 30303) (time+50)
         
  return sock
         
pointToBytes::Point->[Word8]
pointToBytes (Point x y) = intToBytes x ++ intToBytes y
pointToBytes PointO = error "pointToBytes got value PointO, I don't know what to do here"
         
udpHandshakeServer :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m, MonadIO m) 
                   => H.PrvKey 
                   -> Socket
                   -> m ()
udpHandshakeServer prv sock = do
   (msg,addr) <- liftIO $ NB.recvFrom sock 1280  -- liftIO unavoidable?

   let (packet, otherPubkey) = dataToPacket msg
   liftIO $ putStrLn $ "Message Received from " ++ show (B16.encode $ B.pack $ pointToBytes $ hPubKeyToPubKey otherPubkey)

   --liftIO $ putStrLn $ "Message Received from " ++ (show $ B16.encode $ B.pack $ pointToBytes $ hPubKeyToPubKey $ H.derivePubKey $ fromMaybe (error "invalid private number in main") $ H.makePrvKey $ fromIntegral otherPubkey)

   liftIO $ putStrLn $ "     --" ++ format (fst $ dataToPacket msg)

   case packet of
     Ping _ _ _ _ -> do
                 let ip = sockAddrToIP addr
                 curTime <- liftIO $ getCurrentTime
                 let peer = PPeer {
                                pPeerPubkey = hPubKeyToPubKey $ otherPubkey,
                                pPeerIp = T.pack ip,
                                pPeerPort = fromIntegral $ getAddrPort addr,
                                pPeerNumSessions = 0,
                                pPeerLastTotalDifficulty = 0,
                                pPeerLastMsg  = T.pack "msg",
                                pPeerLastMsgTime = curTime,
                                pPeerLastBestBlockHash = SHA 0,
                                pPeerVersion = T.pack "61" -- fix
                              }
                 _ <- addPeer peer
        
                 time <- liftIO $ round `fmap` getPOSIXTime
                 peerAddr <- fmap IPV4Addr $ liftIO $ inet_addr "127.0.0.1"
                 liftIO $ sendPacket sock prv addr $ Pong (Endpoint peerAddr 30303 30303) 4 (time+50)

     Pong _ _ _ -> do
                 time <- liftIO $ round `fmap` getPOSIXTime
                 liftIO $ sendPacket sock prv addr $ FindNeighbors (NodeID $ B.pack $ pointToBytes $ hPubKeyToPubKey otherPubkey) (time + 50)
                 --liftIO $ sendPacket sock prv addr $ FindNeighbors (NodeID $ B.pack $ pointToBytes $ hPubKeyToPubKey $ H.derivePubKey prv) (time + 50)

     FindNeighbors _ _ -> do
                 time <- liftIO $ round `fmap` getPOSIXTime
                 liftIO $ sendPacket sock prv addr $ Neighbors [] (time + 50)
                 liftIO $ sendPacket sock prv addr $ FindNeighbors (NodeID $ B.pack $ pointToBytes $ hPubKeyToPubKey otherPubkey) (time + 50)
                        
     Neighbors neighbors _ -> do
                 forM_ neighbors $ \(Neighbor (Endpoint addr' _ tcpPort) nodeID) -> do
                                curTime <- liftIO $ getCurrentTime
                                let peer = PPeer {
                                             pPeerPubkey = nodeIDToPoint nodeID,
                                             pPeerIp = T.pack $ format addr',
                                             pPeerPort = fromIntegral tcpPort,
                                             pPeerNumSessions = 0,
                                             pPeerLastTotalDifficulty = 0,
                                             pPeerLastMsg  = T.pack "msg",
                                             pPeerLastMsgTime = curTime,
                                             pPeerLastBestBlockHash = SHA 0,
                                             pPeerVersion = T.pack "61" -- fix
                                           }
                                _ <- addPeer peer
                     
                                return ()

                        
   udpHandshakeServer prv sock

getAddrPort::SockAddr->PortNumber
getAddrPort (SockAddrInet portNumber _) = portNumber
getAddrPort (SockAddrInet6 portNumber _ _ _) = portNumber
getAddrPort _ = error $ "getAddrPort called for address that doesn't have a port"
