{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Prelude hiding (FilePath)

parser :: Parser FilePath
parser = argPath "SRC_DIR"  "A source directory to find an images to import"

main :: IO ()
main = do
  sourceDir <- options "Generates an .sql file to use for importing images meta info into DB" parser
  printf ("Got directory "%fp%"\n") sourceDir
