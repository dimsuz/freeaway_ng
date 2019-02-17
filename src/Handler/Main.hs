{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Main where

import Import

getMainR :: Handler Html
getMainR = do
  imageCount <- runDB $ count ([] :: [Filter ExpressionImage])
  exprCount <- runDB $ count ([] :: [Filter Expression])
  defaultLayout $ do
    $(widgetFile "mainpage_new")

getRandomImage :: App -> FilePath
getRandomImage master =
  let staticDir = appStaticDir $ appSettings master
  in staticDir </> "img" </> "gallery"
