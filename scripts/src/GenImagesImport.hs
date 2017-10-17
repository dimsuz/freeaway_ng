{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (encodeString)
import qualified Control.Foldl as Fold
import qualified Data.Text as T

data ImageMetaData = ImageMetaData
  {
    fileName :: Text
  , width :: Int
  , height :: Int
  }
  deriving Show

parser :: Parser FilePath
parser = argPath "SRC_DIR"  "A source directory to find an images to import"

extractImageData :: FilePath -> ImageMetaData
extractImageData fp = ImageMetaData (T.pack $ encodeString fp) 0 0

extract :: Text -> IO (Maybe Line)
extract t = single (inproc "file" [t] empty)

main :: IO ()
main = do
  sourceDir <- options "Generates an .sql file to use for importing images meta info into DB" parser
  files <- view (fmap extractImageData (ls sourceDir))
  fileNames <- fold (fmap (fileName . extractImageData)  (ls sourceDir)) Fold.list
  -- printf ("Got directory "%fp%"\n") sourceDir
  output <- mapM extract $ take 3 fileNames
  print output
