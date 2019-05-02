{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Main where

import Import
import qualified Data.List as L ((!!))
import System.Random (randomRIO)

getMainR :: Handler Html
getMainR = do
  image <- findRandomImage
  expression <- findRandomExpr
  exprCount <- runDB $ count ([] :: [Filter Expression])
  defaultLayout $(widgetFile "mainpage_new")

findRandomImage :: HandlerT App IO ExpressionImage
findRandomImage = do
  recordKeys <- runDB $ selectKeysList ([] :: [Filter ExpressionImage]) []
  when (null recordKeys) (sendResponseStatus status500 ("no expressions available" :: Text))
  recordIndex <- liftIO $ randomRIO (0, length recordKeys - 1)
  let key = recordKeys L.!! recordIndex
  runDB $ get404 key

findRandomExpr :: HandlerT App IO Expression
findRandomExpr = do
  recordKeys <- runDB $ selectKeysList ([] :: [Filter Expression]) []
  when (null recordKeys) (sendResponseStatus status500 ("no expressions available" :: Text))
  recordIndex <- liftIO $ randomRIO (0, length recordKeys - 1)
  let key = recordKeys L.!! recordIndex
  runDB $ get404 key
