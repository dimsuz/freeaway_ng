{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Import
import Model
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Database.Persist.Sqlite              (sqlDatabase, wrapConnection, createSqlPool, runSqlPool)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger (runStderrLoggingT)

rawConnection :: Text -> IO Sqlite.Connection
rawConnection = Sqlite.open

main :: IO ()
main = do
  settings <- loadYamlSettings
    ["config/settings.yml"]
    []
    useEnv
  sqliteConn <- rawConnection (sqlDatabase $ appDatabaseConf settings)
  pool <- runStderrLoggingT (createSqlPool (wrapConnection sqliteConn) 1)
  runSqlPool (runMigration migrateAll) pool
  return ()
