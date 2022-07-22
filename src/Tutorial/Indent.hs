{-# LANGUAGE TupleSections #-}

module Tutorial.Indent
  ( pItemList
  , sampleIL
  ) where

import Control.Applicative hiding (some)
import Control.Monad       (void)

import qualified Data.Text as T
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

lc :: Parser ()
lc = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lc empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lc empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p where
  p = do
    header <- pItem
    return (L.IndentMany Nothing (return . (header, )) pLineFold)

pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
  let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
  in unwords <$> ps <* scn

pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p) where
  p = do
    header <- pItem
    return (L.IndentSome Nothing (return . (header, )) pComplexItem)

sampleIL :: T.Text
sampleIL = T.unlines
  [ "first-chapter"
  , "  paragraph-one"
  , "    note-A # an important note here!"
  , "    note-B"
  , "  paragraph-two"
  , "    note-1"
  , "    note-2"
  , "  paragraph-three"
  ]
