{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = do
  defaultLayout $ do
    $(widgetFile "mainpage")

getCharterR :: Handler Html
getCharterR = do
  defaultLayout $ do
    $(widgetFile "mainpage")

getJoinRulesR :: Handler Html
getJoinRulesR = do
  defaultLayout $ do
    $(widgetFile "mainpage")