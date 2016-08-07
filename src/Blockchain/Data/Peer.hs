{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
    
module Blockchain.Data.Peer where

import Control.Monad.Logger
import Crypto.Types.PubKey.ECC
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Database.Persist.Postgresql as SQL
import Database.Persist.TH
import Text.Regex.PCRE


import Blockchain.Data.PersistTypes ()
import Blockchain.Data.PubKey
import Blockchain.EthConf
import Blockchain.MiscJSON ()
import Blockchain.SHA

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PPeer 
    pubkey Point Maybe
    ip T.Text
    port Int
    numSessions Int
    lastMsg T.Text
    lastMsgTime UTCTime
    enableTime UTCTime
    lastTotalDifficulty Integer
    lastBestBlockHash SHA
    bondState Int
    version T.Text
    deriving Show Read Eq
|]

jamshidBirth::UTCTime
jamshidBirth = posixSecondsToUTCTime 0

createPeer::String->PPeer
createPeer peerString = 
  PPeer {
    pPeerPubkey = Just $ stringToPoint pubKey,
    pPeerIp = T.pack ip,
    pPeerPort = port',
    pPeerNumSessions = 0,
    pPeerLastTotalDifficulty = 0,
    pPeerLastMsg  = T.pack "msg",
    pPeerLastMsgTime = jamshidBirth,
    pPeerEnableTime = jamshidBirth,
    pPeerLastBestBlockHash = SHA 0,
    pPeerBondState=0,
    pPeerVersion = T.pack "61" -- fix
    }
  where
    (pubKey, ip, port') = parseEnode peerString

parseEnode::String->(String, String, Int)
parseEnode enode =
  case (enode =~ ("enode://([a-f0-9]{128})@(\\d+\\.\\d+\\.\\d+\\.\\d+):(\\d+)"::String))::(String, String, String, [String]) of
    ("", _, "", [pubKey, ip, port']) -> (pubKey, ip, read port')
    _ -> error $ "malformed enode: " ++ enode

getAvailablePeers::IO [PPeer]
getAvailablePeers = do
  currentTime <- getCurrentTime
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  fmap (map SQL.entityVal) $ flip SQL.runSqlPool sqldb $ 
    SQL.selectList [PPeerEnableTime SQL.<. currentTime] []

setPeerBondingState::String->Int->Int->IO ()
setPeerBondingState ip port' state = do
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  flip SQL.runSqlPool sqldb $ 
    SQL.updateWhere [PPeerIp SQL.==. T.pack ip, PPeerPort SQL.==. port'] [PPeerBondState SQL.=. state]
  return ()
  
getBondedPeers::IO [PPeer]
getBondedPeers = do
  currentTime <- getCurrentTime
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  fmap (map SQL.entityVal) $ flip SQL.runSqlPool sqldb $ 
    SQL.selectList [PPeerBondState SQL.==. 2, PPeerEnableTime SQL.<. currentTime] []

getUnbondedPeers::IO [PPeer]
getUnbondedPeers = do
  currentTime <- getCurrentTime
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  fmap (map SQL.entityVal) $ flip SQL.runSqlPool sqldb $ 
    SQL.selectList [PPeerBondState SQL.==. 0, PPeerEnableTime SQL.<. currentTime] []

defaultPeer::PPeer
defaultPeer = PPeer{
  pPeerPubkey=Nothing,
  pPeerIp="",
  pPeerPort=30303,
  pPeerNumSessions=0,
  pPeerLastMsg="",
  pPeerLastMsgTime=posixSecondsToUTCTime 0,
  pPeerEnableTime=posixSecondsToUTCTime 0,
  pPeerLastTotalDifficulty=0,
  pPeerLastBestBlockHash=SHA 0,
  pPeerBondState=0,
  pPeerVersion=""
  }

disablePeerForSeconds::PPeer->Int->IO ()
disablePeerForSeconds peer seconds = do
  currentTime <- getCurrentTime
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  flip SQL.runSqlPool sqldb $ 
    SQL.updateWhere [PPeerIp SQL.==. pPeerIp peer, PPeerPort SQL.==. pPeerPort peer] [PPeerEnableTime SQL.=. fromIntegral seconds `addUTCTime` currentTime]
  return ()
  
