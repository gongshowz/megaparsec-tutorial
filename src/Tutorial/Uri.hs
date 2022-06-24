module Tutorial.Uri
  ( Parser(..)
  , Scheme(..)
  , Uri(..)
  , pUri
  ) where

import Control.Monad

import Data.Text as T
import Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug

type Parser = Parsec Void Text

data Uri = Uri
  { uriScheme   :: Scheme
  , uriUser     :: Maybe (Text, Text)
  , uriHost     :: Text
  , uriPort     :: Maybe Int
  , uriPath     :: Maybe Text
  , uriQuery    :: Maybe [(Text, Text)]
  , uriFragment :: Maybe Text
  } deriving (Eq, Show)

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto"
  ]

pUser :: Parser (Text, Text)
pUser = do
  user <- T.pack <$> some alphaNumChar <?> "username"
  void (char ':')
  password <- T.pack <$> some alphaNumChar <?> "password"
  return (user, password)

pQueryPair :: Parser (Text, Text)
pQueryPair = do
  key <- T.pack <$> some alphaNumChar <?> "query key"
  void (char '=')
  val <- T.pack <$> some alphaNumChar <?> "query value"
  return (key, val)

pQuery :: Parser [(Text, Text)]
pQuery = some $ do
  pair <- pQueryPair
  optional (char '&')
  return pair

pUri :: Parser Uri
pUri = do
  scheme <- pScheme <?> "valid scheme"
  void (string "://")
  user <- optional . try $ pUser
  optional (char '@')
  host <- T.pack <$> some (alphaNumChar <|> char '.')
  port <- optional (char ':' *> label "port number" L.decimal)
  optional (char '/')
  path <- optional . try $ T.pack <$> some (alphaNumChar <|> char '/')
  optional (char '?')
  query <- optional . try $ pQuery
  optional (char '#')
  fragment <- optional . try $ T.pack <$> some alphaNumChar
  return $ Uri scheme user host port path query fragment

