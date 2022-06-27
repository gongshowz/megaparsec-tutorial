module Tutorial.Uri
  ( Parser(..)
  , Scheme(..)
  , Uri(..)
  , pUri
  ) where

import Control.Monad

import Data.Char (isAlphaNum)
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
  user <- takeWhile1P (Just "username") isAlphaNum
  void (char ':')
  password <- takeWhile1P (Just "password") isAlphaNum
  return (user, password)

pQueryPair :: Parser (Text, Text)
pQueryPair = do
  key <- takeWhile1P (Just "query key") isAlphaNum
  void (char '=')
  val <- takeWhile1P (Just "query value") isAlphaNum
  return (key, val)

pQuery :: Parser [(Text, Text)]
pQuery = some $ do
  pair <- pQueryPair
  optional (char '&')
  return pair

alphaNumOr :: Char -> Char -> Bool
alphaNumOr a b = a == b || isAlphaNum b

pUri :: Parser Uri
pUri = do
  scheme <- pScheme <?> "valid scheme"
  void (string "://")
  user <- optional . try $ pUser
  optional (char '@')
  host <- takeWhile1P (Just "host") (alphaNumOr '.')
  port <- optional (char ':' *> label "port number" L.decimal)
  optional (char '/')
  path <- optional . try $ takeWhile1P (Just "path") (alphaNumOr '/')
  optional (char '?')
  query <- optional . try $ pQuery
  optional (char '#')
  fragment <- optional . try $ takeWhile1P (Just "fragment") isAlphaNum
  return $ Uri scheme user host port path query fragment

