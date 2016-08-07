{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
    
module Blockchain.Data.Peer where

import Crypto.Types.PubKey.ECC
import Data.Text
import Data.Time
import Database.Persist.TH

import Blockchain.Data.PersistTypes ()
import Blockchain.MiscJSON ()
import Blockchain.SHA

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PPeer 
    pubkey Point Maybe
    ip Text
    port Int
    numSessions Int
    lastMsg Text
    lastMsgTime UTCTime
    enableTime UTCTime
    lastTotalDifficulty Integer
    lastBestBlockHash SHA
    version Text
    deriving Show Read Eq
|]

addPeer'::PPeer->IO ()
addPeer' = undefined
