{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Meetings where

import Import

getMeetingsR :: Handler Html
getMeetingsR = do
  defaultLayout $ do
    $(widgetFile "mainpage")
