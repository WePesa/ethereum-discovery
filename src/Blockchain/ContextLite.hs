{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.ContextLite (
  ContextLite(..),
  initContextLite,
  addPeer,
  ) where


import Control.Monad.State
import Control.Monad.Trans.Resource

import Blockchain.DBM
import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs

import qualified Database.Persist.Postgresql as SQL
import qualified Database.PostgreSQL.Simple as PS

import qualified Data.Text as T

data ContextLite =
  ContextLite {
    liteSQLDB::SQLDB,
    notifHandler::PS.Connection,
    debugEnabled::Bool
  } deriving Show


instance Show PS.Connection where
  show _ = "Postgres Simple Connection"

type ContextMLite = StateT ContextLite (ResourceT IO)

instance HasSQLDB ContextMLite where
  getSQLDB = fmap liteSQLDB get

initContextLite :: (MonadResource m, MonadIO m, MonadBaseControl IO m) => SQL.ConnectionString -> m ContextLite
initContextLite _ = do
  notif <- liftIO $ PS.connect PS.defaultConnectInfo {   -- bandaid, should eventually be added to monad class
            PS.connectPassword = "api",
            PS.connectDatabase = "eth"
           }
  dbs <- openDBs
  return ContextLite {
                    liteSQLDB = sqlDB' dbs,                    
                    notifHandler=notif,
                    debugEnabled = False
                 }

addPeer :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>PPeer->m (SQL.Key PPeer)
addPeer peer = do
  db <- getSQLDB
  maybePeer <- getPeerByIP (T.unpack $ pPeerIp peer)
  runResourceT $
    SQL.runSqlPool (actions maybePeer) db
  where actions mp = case mp of
            Nothing -> do
              peerid <- SQL.insert $ peer        
              return peerid
  
            Just peer'-> do 
              SQL.update (SQL.entityKey peer') [PPeerPubkey SQL.=.(pPeerPubkey peer)]  
              return (SQL.entityKey peer')

getPeerByIP :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>String->m (Maybe (SQL.Entity PPeer))
getPeerByIP ip = do
  db <- getSQLDB
  entPeer <- runResourceT $ SQL.runSqlPool actions db
  
  case entPeer of 
    [] -> return Nothing
    lst -> return $ Just . head $ lst

  where actions = SQL.selectList [ PPeerIp SQL.==. (T.pack ip) ] []

  

