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
  imageKeys <- runDB $ selectKeysList ([] :: [Filter ExpressionImage]) []
  when (null imageKeys) (sendResponseStatus status500 ("no images available" :: Text))
  imageIndex <- liftIO $ randomRIO (0, length imageKeys - 1)
  let imageKey = imageKeys L.!! imageIndex
  image <- runDB $ get404 imageKey
  exprCount <- runDB $ count ([] :: [Filter Expression])
  defaultLayout $(widgetFile "mainpage_new")
