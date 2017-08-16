{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Biography where

import Import

getNgoMaBiographyR :: Handler Html
getNgoMaBiographyR = do
  defaultLayout $ do
    $(widgetFile "mainpage")
