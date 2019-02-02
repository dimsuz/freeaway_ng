{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Import
import Model
import qualified Control.Exception as Exception
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Database.Persist.Sqlite              (sqlDatabase, wrapConnection, createSqlPool, runSqlPool)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Data.Yaml                   (decodeFileEither, ParseException, withText)

instance FromJSON Expression where
  parseJSON = withText "Expression" $ \t -> return (Expression t)

rawConnection :: Text -> IO Sqlite.Connection
rawConnection = Sqlite.open

loadExpressions :: MonadIO m => FilePath -> m [Expression]
loadExpressions filepath = do
  decodeResult <- liftIO $ decodeFileEither filepath
  return $ either Exception.throw id decodeResult

insertExpression :: MonadIO m => Expression -> ReaderT SqlBackend m ()
insertExpression expr = insert_ expr

seed :: MonadIO m => ReaderT SqlBackend m ()
seed = do
  deleteWhere ([] :: [Filter Expression])
  expressions <- loadExpressions "config/expressions.yml"
  sequence_ $ map insert_ expressions

main :: IO ()
main = do
  settings <- loadYamlSettings
    ["config/settings.yml"]
    []
    useEnv
  sqliteConn <- rawConnection (sqlDatabase $ appDatabaseConf settings)
  pool <- runStderrLoggingT (createSqlPool (wrapConnection sqliteConn) 1)
  runSqlPool ((runMigration migrateAll) >> seed) pool
  return ()
