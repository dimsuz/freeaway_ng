{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Art where

import Import

getPoetryR :: Handler Html
getPoetryR = do
  defaultLayout $ do
    $(widgetFile "mainpage")

getPaintingR :: Handler Html
getPaintingR = do
  defaultLayout $ do
    $(widgetFile "mainpage")

getYantrasR :: Handler Html
getYantrasR = do
  defaultLayout $ do
    $(widgetFile "mainpage")
