{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.PeerDB where

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Blockchain.Data.DataDefs
    
import           Control.Monad.Logger    (runNoLoggingT)
import qualified Database.Persist.Postgresql as SQL
import Blockchain.EthConf
import Blockchain.SHA

getNumAvailablePeers::IO Int
getNumAvailablePeers = do
  currentTime <- getCurrentTime
  sqldb <- runNoLoggingT $ SQL.createPostgresqlPool connStr' 20
  fmap length $ flip SQL.runSqlPool sqldb $ 
    SQL.selectList [PPeerEnableTime SQL.<. currentTime] []

