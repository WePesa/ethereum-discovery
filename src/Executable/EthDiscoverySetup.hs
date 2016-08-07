{-# LANGUAGE OverloadedStrings #-}

module Executable.EthDiscoverySetup (
  setup
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Crypto.Types.PubKey.ECC
import Database.Persist.Postgresql
import qualified Data.Text as T
import Data.Time.Clock

import Blockchain.Data.DataDefs
import Blockchain.EthConf
import Blockchain.SHA

setup::LoggingT IO ()
setup = do
  curTime <- liftIO $ getCurrentTime
  let peer =
        PPeer {
          pPeerPubkey = Just $ Point 1 2,
          pPeerIp = "1.2.3.4",
          pPeerPort = 30303,
          pPeerNumSessions = 0,
          pPeerLastTotalDifficulty = 0,
          pPeerLastMsg  = T.pack "msg",
          pPeerLastMsgTime = curTime,
          pPeerEnableTime = curTime,
          pPeerLastBestBlockHash = SHA 0,
          pPeerVersion = T.pack "61" -- fix
          }

  runNoLoggingT $ withPostgresqlConn connStr $ runSqlConn $ do
    _ <- insert peer
    return ()
