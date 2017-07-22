{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Main where

import Import

getMainR :: Handler Html
getMainR = do
  defaultLayout $ do
    $(widgetFile "mainpage")
