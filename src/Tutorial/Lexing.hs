module Tutorial.Lexing
  ( pExpr
  , sampleExpr
  ) where

import Control.Monad
import Control.Monad.Combinators.Expr

import Data.Text as T
import Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Top level parser type
type Parser = Parsec Void Text

-- AST for expression parsing
data Expr
  = Var Text
  | Int Int
  | Double Double
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Ord, Show)

-- Line comment type
slc :: Parser ()
slc = L.skipLineComment "--"

-- Block comment type
sbc :: Parser ()
sbc = L.skipBlockComment "{-" "-}"

-- Single quote parser
singleQuote :: Parser Char
singleQuote = char '\''

-- Double quote parser
doubleQuote :: Parser Char
doubleQuote = char '\"'

-- A parser for whitespace and comments
sc :: Parser ()
sc = L.space space1 slc sbc

-- A lexeme wrapper that picks up all trailing white space using the supplied space consumer.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Matches text and picks up all trailing white space.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parse character literals
pChar :: Parser Char
pChar = between singleQuote singleQuote L.charLiteral

-- Parse string literals
pString :: Parser Text
pString = do
  let s = doubleQuote *> manyTill L.charLiteral doubleQuote
  T.pack <$> s

-- Parse integer literals
pInt :: Parser Int
pInt = lexeme L.decimal

-- Parse float literals
pDouble :: Parser Double
pDouble = lexeme L.float

-- Parse signed integer literals
pIntSigned :: Parser Int
pIntSigned = L.signed sc pInt

-- Parse signed float literals
pDoubleSigned :: Parser Double
pDoubleSigned = L.signed sc pDouble

-- Parse a keyword
pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme $
  string keyword <* notFollowedBy alphaNumChar

-- Parse a variable expression
pVariable :: Parser Expr
pVariable = Var . T.pack <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

-- Parse an integer expression
pIntExpr :: Parser Expr
pIntExpr = Int <$> pInt

-- Parse a floating point expression
pDoubleExpr :: Parser Expr
pDoubleExpr = Double <$> pDouble

-- Parse and expression in parens
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parse a float or integer expression
pNumeric :: Parser Expr
pNumeric =  try pDoubleExpr <|> pIntExpr

-- Term parser
pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pNumeric
  ]

-- Operator table. Ordered in descending precedence, so the higher we place a group of operators
-- in it, the tighter they bind
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Neg
    , prefix "+" id
    ]
  , [ binary "*" Mul
    , binary "/" Div
    ]
  , [ binary "+" Add
    , binary "-" Sub
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL $ f <$ symbol name

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix $ f <$ symbol name

postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
postfix name f = Postfix $ f <$ symbol name

-- Expression parser
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- Sample expression to parse
sampleExpr :: Text
sampleExpr = T.unlines lines where
  lines =
    [ "-1        -- line one"
    , "* (a + 3) -- line two"
    , "/ (2 - b) -- line three"
    ]
