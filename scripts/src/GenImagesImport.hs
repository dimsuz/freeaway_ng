{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (encodeString)
import Data.Maybe
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as PS

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

parseFileOutput :: Line -> Maybe (Int, Int)
parseFileOutput line = let s = lineToText line
                           parsed = P.parse fileOutputParser "" (lineToText line)
                       in case parsed of
                            Left _ -> Nothing
                            Right dimens -> dimens

fileOutputParser :: PS.Parser (Maybe (Int, Int))
fileOutputParser = (join . safeHead . (filter isJust)) <$> dimensParser1 `P.sepBy` (P.char ',' >> P.spaces)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

dimensParser1 :: PS.Parser (Maybe (Int, Int))
dimensParser1 = P.try (Just <$> dimensParser) <|> (P.skipMany (P.noneOf ",") >> pure Nothing)

dimensParser :: PS.Parser (Int, Int)
dimensParser = do
  w <- P.many1 P.digit
  P.spaces
  P.char 'x'
  P.spaces
  h <- P.many1 P.digit
  return (read w, read h)

extract :: Text -> IO (Maybe Line)
extract t = single (inproc "file" [t] empty)

main :: IO ()
main = do
  sourceDir <- options "Generates an .sql file to use for importing images meta info into DB" parser
  files <- view (fmap extractImageData (ls sourceDir))
  fileNames <- fold (fmap (fileName . extractImageData)  (ls sourceDir)) Fold.list
  -- printf ("Got directory "%fp%"\n") sourceDir
  output <- catMaybes <$> mapM extract fileNames
  print $ map parseFileOutput output
