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
  -- TODO selectKeys, take list length, select random key, then use selectFirst
  imageCount <- runDB $ count ([] :: [Filter ExpressionImage])
  imageIndex <- liftIO $ randomRIO (0, imageCount - 1)
  images <- runDB $ selectList ([] :: [Filter ExpressionImage]) []
  let (Entity _ image) = images L.!! imageIndex
  exprCount <- runDB $ count ([] :: [Filter Expression])
  defaultLayout $(widgetFile "mainpage_new")
