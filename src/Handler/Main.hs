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
    master <- getYesod
    let variable = getRandomImage master
    $(widgetFile "mainpage_new")

getRandomImage :: App -> FilePath
getRandomImage master =
  let staticDir = appStaticDir $ appSettings master
  in staticDir </> "img"
