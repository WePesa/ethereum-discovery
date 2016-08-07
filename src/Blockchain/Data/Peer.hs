{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
    
module Blockchain.Data.Peer where

import Crypto.Types.PubKey.ECC
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Database.Persist.TH
import Text.Regex.PCRE


import Blockchain.Data.PersistTypes ()
import Blockchain.Data.PubKey
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
    pPeerPort = port,
    pPeerNumSessions = 0,
    pPeerLastTotalDifficulty = 0,
    pPeerLastMsg  = T.pack "msg",
    pPeerLastMsgTime = jamshidBirth,
    pPeerEnableTime = jamshidBirth,
    pPeerLastBestBlockHash = SHA 0,
    pPeerVersion = T.pack "61" -- fix
    }
  where
    (pubKey, ip, port) = parseEnode peerString

parseEnode::String->(String, String, Int)
parseEnode enode =
  case (enode =~ ("enode://([a-f0-9]{128})@(\\d+\\.\\d+\\.\\d+\\.\\d+):(\\d+)"::String))::(String, String, String, [String]) of
    ("", _, "", [pubKey, ip, port]) -> (pubKey, ip, read port)
    _ -> error $ "malformed enode: " ++ enode

addPeer'::PPeer->IO ()
addPeer' = undefined
