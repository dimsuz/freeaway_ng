{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (encodeString)

data ImageMetaData = ImageMetaData
  {
    fileName :: String
  , width :: Int
  , height :: Int
  }
  deriving Show

parser :: Parser FilePath
parser = argPath "SRC_DIR"  "A source directory to find an images to import"

extractImageData :: FilePath -> ImageMetaData
extractImageData fp = ImageMetaData (encodeString fp) 0 0

main :: IO ()
main = do
  sourceDir <- options "Generates an .sql file to use for importing images meta info into DB" parser
  files <- view (fmap extractImageData (ls sourceDir))
  printf ("Got directory "%fp%"\n") sourceDir
