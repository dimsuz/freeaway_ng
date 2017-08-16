{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Articles where

import Import

getNgoMaArticlesR :: Handler Html
getNgoMaArticlesR = do
  defaultLayout $ do
    $(widgetFile "mainpage")

getMemberArticlesR :: Handler Html
getMemberArticlesR = do
  defaultLayout $ do
    $(widgetFile "mainpage")
