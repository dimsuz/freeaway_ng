{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Control.Foldl as Fold
import Data.Maybe
import Data.Aeson
import GHC.Generics
import Control.Monad (filterM)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.ByteString.Lazy as B

data ViewBindings = ViewBindings { controllerFile :: FilePath
                                 , layoutFile :: FilePath
                                 , bindings :: [Binding]
                                 }
  deriving (Eq, Show, Generic)

-- property name, property type, property layout id
data Binding = Binding { propertyName :: Text
                       , propertyType :: Text
                       , propertyLayoutId :: Text
                       }
  deriving (Eq, Show, Generic)

instance ToJSON Binding where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Binding

instance ToJSON ViewBindings where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ViewBindings

instance ToJSON FilePath where
  toJSON fp = case FP.toText fp of
    Right v -> String v
    Left v -> error "Failed to encode file path"

instance FromJSON FilePath where
  parseJSON = withText "FilePath" $ \v -> return (fromText v)

-- pasted from foldl-1.3.5 to not mess with stack for now
prefilter :: (a -> Bool) -> Fold a r -> Fold a r
prefilter f (Fold step begin done) = Fold step' begin done
  where
    step' x a = if f a then step x a else x
{-# INLINABLE prefilter #-}

-- converts Document to document, but 
unTitle :: T.Text -> T.Text
unTitle s = T.toLower (T.take 1 s) <> T.tail s

snakeToCamel :: T.Text -> T.Text
snakeToCamel t = case T.splitOn "_" t of
  [] -> t
  (w:ws) -> T.concat (unTitle w : map T.toTitle ws)

kotlinIdPattern :: Pattern Text
kotlinIdPattern = do
  rid <- text "R.id."
  identifier <- plus (alphaNum <|> char '_')
  return $ rid <> (snakeToCamel identifier)

bindViewPattern :: Pattern Binding
bindViewPattern = do
  text "val"
  plus space
  propName <- plus alphaNum
  char ':'
  plus space
  propType <- plus alphaNum
  plus space
  text "by"
  plus space
  text "BindView(R.id."
  propLayoutId <- plus (alphaNum <|> char '_')
  char ')'
  return $ Binding propName  propType  propLayoutId

-- finds a layout identifier for controller views
viewLayoutPattern :: Pattern Text
viewLayoutPattern = do
  text "inflater.inflate(R.layout."
  layoutId <- text "content_"  <> plus (alphaNum <|> char '_')
  char ','
  return layoutId

-- finds a layout "@id"/"@+id" attributes, returns only name part,
-- i.e. "@+id/some_id" -> "some_id"
idAttrPattern :: Pattern Text
idAttrPattern = do
  char '@'
  optional (char '+')
  text "id/"
  propLayoutId <- plus (alphaNum <|> char '_')
  return propLayoutId

replaceLayoutIdPattern :: Text -> Text -> Pattern Text
replaceLayoutIdPattern origin replacemnt = do
  idpart <- text "@id/" <|> text "@+id/"
  text origin
  char '"'
  return $ idpart <> replacemnt <> "\""

replaceControllerIdPattern :: Text -> Text -> Pattern Text
replaceControllerIdPattern origin replacemnt = do
  idpart <- text "R.id."
  text origin
  -- this is to guard against replacing
  -- "button_short" for "buttonShort" in "button_short_something" (leading to "buttonShort_something")
  n <- noneOf "_"
  return $ idpart <> replacemnt `T.snoc` n

replaceControllerSnakePattern :: Pattern Text
replaceControllerSnakePattern = do
  idpart <- text "R.id."
  propLayoutId <- plus (alphaNum <|> char '_')
  -- this is to guard against replacing
  -- "button_short" for "buttonShort" in "button_short_something" (leading to "buttonShort_something")
  n <- noneOf "_"
  return $ "R.id." <> (snakeToCamel propLayoutId) `T.snoc` n

replaceLayoutSnakePattern :: Pattern Text
replaceLayoutSnakePattern = do
  idpart <- text "@id/" <|> text "@+id/"
  propLayoutId <- plus (alphaNum <|> char '_')
  char '"'
  return $ idpart <> (snakeToCamel propLayoutId) <> "\""

argsParser :: Parser FilePath
argsParser = argPath "SRC_DIR"  "A source directory to find file to refactor"

nonNullShell :: Shell a -> IO Bool
nonNullShell sh = not <$> fold sh Fold.null

findLayoutFileById :: Text -> FilePath -> IO (Maybe FilePath)
findLayoutFileById lid dir = fold (find (suffix layoutFileName) dir) $ prefilter (not . isInBuildDir) Fold.head
  where layoutFileName = text (lid <> ".xml")
        isInBuildDir fp = any (\p -> p == fromText "build/") $ splitDirectories fp

extractLayoutFilePath :: FilePath -> FilePath -> IO (Maybe FilePath)
extractLayoutFilePath dir fp = do
  let inputShell = input fp
  let grepShell = grep (has viewLayoutPattern) inputShell
  let sedShell = sed (has viewLayoutPattern) grepShell
  layoutId <- fold sedShell Fold.head
  case layoutId of
    Just id -> findLayoutFileById (lineToText id) dir
    Nothing -> return Nothing

extractViewBindings :: FilePath -> IO [Binding]
extractViewBindings fp = do
  let inputShell = input fp
  let grepShell = lineToText <$> grep (has bindViewPattern) inputShell
  let matchFirst p t = head $ match p t
  let sedShell = matchFirst (has bindViewPattern) <$> grepShell
  fold sedShell Fold.list

readRefactorData :: FilePath -> IO [ViewBindings]
readRefactorData fp = (fromJust . decode) <$> B.readFile (FP.encodeString fp)

filesWithPattern :: FilePath -> Pattern a -> Pattern b -> IO [FilePath]
filesWithPattern sourceDir namePattern contentPattern = do
  files <- fold (find namePattern sourceDir) Fold.list
  let fileShells = map (\filePath -> (filePath, grep contentPattern (input filePath))) files
  filtered <- filterM (\(_, sh) -> nonNullShell sh) fileShells
  return $ map fst filtered

gatherRefactorData :: FilePath -> FilePath -> IO ()
gatherRefactorData sourceDir filePath = do
  filesWithBinds <- filesWithPattern sourceDir (suffix ".kt") (has bindViewPattern)
  layoutFilesMaybes <- sequence $ map (extractLayoutFilePath sourceDir) filesWithBinds
  viewBindings <- sequence $ map extractViewBindings filesWithBinds
  let layoutFiles = map fromJust layoutFilesMaybes
  let bindings = zipWith3 (\cf lf vb -> ViewBindings cf lf vb) filesWithBinds layoutFiles viewBindings
  do if (length filesWithBinds /= length bindings) then error "not all bindings found!" else return ()
  putStrLn ("found " ++ (show (length bindings)) ++ " files with bindings")
  B.writeFile (FP.encodeString filePath) (encode bindings)
  return ()

replaceBindings :: (Text -> Text -> Pattern Text) -> FilePath -> [Binding] -> IO ()
replaceBindings p layoutFile idBindings = do
  sequence_ $ map (\b -> inplace (p (propertyLayoutId b) (propertyName b)) layoutFile) idBindings

replaceLayoutIds :: FilePath -> [Binding] -> IO ()
replaceLayoutIds fp bs = do
  replaceBindings replaceLayoutIdPattern fp bs
  inplace replaceLayoutSnakePattern fp

replaceControllerIds :: FilePath -> [Binding] -> IO ()
replaceControllerIds fp bs = do
  replaceBindings replaceControllerIdPattern fp bs
  inplace replaceControllerSnakePattern fp

performRefactor :: ViewBindings -> IO ()
performRefactor (ViewBindings controllerFile layoutFile idBindings) = do
  putStrLn (show layoutFile)
  replaceLayoutIds layoutFile idBindings
  replaceControllerIds controllerFile idBindings

postRefactorReplaceSnakeWithCamel :: FilePath -> IO ()
postRefactorReplaceSnakeWithCamel sourceDir = do
  ktFiles <- filesWithPattern sourceDir (suffix ".kt") (has replaceControllerSnakePattern)
  xmlFiles <- filesWithPattern sourceDir (suffix ".xml") (has replaceLayoutSnakePattern)
  sequence_ $ map (inplace replaceControllerSnakePattern) ktFiles
  sequence_ $ map (inplace replaceLayoutSnakePattern) xmlFiles

main :: IO ()
main = do
  sourceDir <- options "Refactors BindView delegate to kotlin extensions" argsParser
  -- let filePath = "refactor.json"
  -- gatherRefactorData filePath sourceDir
  -- bindings <- readRefactorData filePath
  -- sequence $ map performRefactor bindings
  postRefactorReplaceSnakeWithCamel sourceDir
  return ()
