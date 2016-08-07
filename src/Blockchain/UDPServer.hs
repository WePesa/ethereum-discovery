{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Blockchain.UDPServer (
      runEthUDPServer,
      connectMe,
      udpHandshakeServer
     ) where

import Network.Socket
import qualified Network.Socket.ByteString as NB
import System.Timeout

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import           Data.Time.Clock.POSIX
import           Data.Time.Clock
import qualified Data.Text as T

import System.Entropy

import qualified Blockchain.Colors as CL
import Blockchain.Data.PubKey
import Blockchain.EthConf
import           Blockchain.Format
import           Blockchain.UDP
import           Blockchain.SHA
import           Blockchain.Data.Peer
import           Blockchain.DB.SQLDB
import           Blockchain.ContextLite
import           Blockchain.P2PUtil
import           Blockchain.PeerDB
                      
import qualified Network.Haskoin.Internals as H
    
runEthUDPServer::(MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m)=>
                 String->String->ContextLite->H.PrvKey->Socket->m ()
runEthUDPServer bootstrapAddr bootstrapPort cxt myPriv sock = do
  _ <- runResourceT $ flip runStateT cxt $ udpHandshakeServer bootstrapAddr bootstrapPort myPriv sock
  return ()

connectMe::(MonadIO m, MonadLogger m)=>
           String->String->H.PrvKey->Int->m Socket
connectMe bootstrapAddr bootstrapPort prv port' = do
--  (serveraddr:_) <- getAddrInfo
--                      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
--                      Nothing (Just (show port))
  (serveraddr:_) <- liftIO $ getAddrInfo
                                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                  Nothing (Just (show port'))
  sock <- liftIO $ socket (addrFamily serveraddr) Datagram defaultProtocol
  liftIO $ bindSocket sock (addrAddress serveraddr)

  time <- liftIO $ round `fmap` getPOSIXTime

--  let bootstrapAddr = "52.16.188.185"
--      bootstrapPort = "30303"
--  let bootstrapAddr = "poc-9.ethdev.com"
--      bootstrapPort = "30303"
--  let bootstrapAddr = "127.0.0.1"
--      bootstrapPort = "30303"
          
  (peeraddr:_) <- liftIO $ getAddrInfo Nothing (Just bootstrapAddr) (Just bootstrapPort)
--  sock2 <- socket (addrFamily peeraddr) Datagram defaultProtocol

  sendPacket sock prv (addrAddress peeraddr) $ Ping 4 (Endpoint (getHostAddress $ addrAddress serveraddr) 30303 30303) (Endpoint (getHostAddress $ addrAddress peeraddr) 30303 30303) (time+50)
         
  return sock
         
addPeersIfNeeded::(MonadIO m, MonadLogger m)=>
                  String->String->H.PrvKey->Socket->m ()
addPeersIfNeeded bootstrapAddr bootstrapPort prv sock= do
  numAvailablePeers <- liftIO getNumAvailablePeers
  when (numAvailablePeers < minAvailablePeers (discoveryConfig ethConf)) $ do
    (peeraddr:_) <- liftIO $ getAddrInfo Nothing (Just bootstrapAddr) (Just bootstrapPort)
    time <- liftIO $ round `fmap` getPOSIXTime
    randomBytes <- liftIO $ getEntropy 64
    sendPacket sock prv (addrAddress peeraddr) $ FindNeighbors (NodeID randomBytes) (time + 50)

udpHandshakeServer::(HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m, MonadIO m, MonadLogger m)=>
                    String->String->H.PrvKey->Socket->m ()
udpHandshakeServer bootstrapAddr bootstrapPort prv sock = do
  addPeersIfNeeded bootstrapAddr bootstrapPort prv sock
  
  maybePacketData <- liftIO $ timeout 10000000 $ NB.recvFrom sock 1280  -- liftIO unavoidable?

  case maybePacketData of
   Nothing -> do
     logInfoN "timeout triggered"
   Just (msg,addr) -> do
     let (packet, otherPubkey) = dataToPacket msg
     logInfoN $ T.pack $ CL.cyan "<<<<" ++ " (" ++ show addr ++ " " ++ BC.unpack (B.take 10 $ B16.encode $ B.pack $ pointToBytes $ hPubKeyToPubKey otherPubkey) ++ "....) " ++ format (fst $ dataToPacket msg)

     case packet of
      Ping _ _ _ _ -> do
                 let ip = sockAddrToIP addr
                 curTime <- liftIO $ getCurrentTime
                 let peer = PPeer {
                       pPeerPubkey = Just $ hPubKeyToPubKey $ otherPubkey,
                       pPeerIp = T.pack ip,
                       pPeerPort = fromIntegral $ getAddrPort addr,
                       pPeerNumSessions = 0,
                       pPeerLastTotalDifficulty = 0,
                       pPeerLastMsg  = T.pack "msg",
                       pPeerLastMsgTime = curTime,
                       pPeerEnableTime = curTime,
                       pPeerLastBestBlockHash = SHA 0,
                       pPeerBondState = 0,
                       pPeerVersion = T.pack "61" -- fix
                       }
                 _ <- addPeer peer
        
                 time <- liftIO $ round `fmap` getPOSIXTime
                 peerAddr <- fmap IPV4Addr $ liftIO $ inet_addr "127.0.0.1"
                 sendPacket sock prv addr $ Pong (Endpoint peerAddr 30303 30303) 4 (time+50)

      Pong _ _ _ -> return ()

      FindNeighbors target _ -> do
                 time <- liftIO $ round `fmap` getPOSIXTime
                 allPeers <- liftIO $ getClosePeers target
                 let firstPeers = take 8 allPeers
                     secondPeers = drop 8 allPeers
                 sendPacket sock prv addr $ Neighbors (map peerToNeighbor firstPeers) (time + 50)
                 sendPacket sock prv addr $ Neighbors (map peerToNeighbor secondPeers) (time + 50)
                        
      Neighbors neighbors _ -> do
                 forM_ neighbors $ \(Neighbor (Endpoint addr' _ tcpPort) nodeID) -> do
                                curTime <- liftIO $ getCurrentTime
                                let peer = PPeer {
                                      pPeerPubkey = Just $ nodeIDToPoint nodeID,
                                      pPeerIp = T.pack $ format addr',
                                      pPeerPort = fromIntegral tcpPort,
                                      pPeerNumSessions = 0,
                                      pPeerLastTotalDifficulty = 0,
                                      pPeerLastMsg  = T.pack "msg",
                                      pPeerLastMsgTime = curTime,
                                      pPeerEnableTime = curTime,
                                      pPeerLastBestBlockHash = SHA 0,
                                      pPeerBondState = 0,
                                      pPeerVersion = T.pack "61" -- fix
                                      }
                                _ <- addPeer peer
                     
                                return ()



                 
  udpHandshakeServer bootstrapAddr bootstrapPort prv sock

getAddrPort::SockAddr->PortNumber
getAddrPort (SockAddrInet portNumber _) = portNumber
getAddrPort (SockAddrInet6 portNumber _ _ _) = portNumber
getAddrPort _ = error $ "getAddrPort called for address that doesn't have a port"
