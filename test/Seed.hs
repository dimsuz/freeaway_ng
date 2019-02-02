{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Import
import Model()
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Database.Persist.Sqlite              (sqlDatabase, wrapConnection, createSqlPool, runSqlPool)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (decode, (.:), withObject)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust)

instance FromJSON Expression where
  parseJSON = withObject "Expression" $ \o -> Expression <$> o .: "text"

rawConnection :: Text -> IO Sqlite.Connection
rawConnection = Sqlite.open

loadExpressions :: MonadIO m => FilePath -> m [Expression]
loadExpressions filepath = do
  decodeResult <- liftIO $ decode <$> B.readFile filepath
  return $ fromJust decodeResult

seed :: MonadIO m => ReaderT SqlBackend m ()
seed = do
  deleteWhere ([] :: [Filter Expression])
  expressions <- loadExpressions "test/seed-data/exprs.json"
  mapM_ insert_ expressions
  liftIO $ putStrLn (pack ("Inserted " <> show (length expressions) <> " expressions"))

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
