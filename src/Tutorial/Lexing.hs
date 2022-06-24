module Tutorial.Lexing where

import Control.Monad
import Control.Monad.Combinators.Expr

import Data.Text as T
import Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

-- AST for expression parsing
data Expr
  = Var Text
  | Int Int
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Ord, Show)

-- Define comment types
slc = L.skipLineComment "--"
sbc = L.skipBlockComment "{-" "-}"

-- Quote types
singleQuote = char '\''
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
pFloat :: Parser Double
pFloat = lexeme L.float

-- Parse signed integer literals
pIntSigned :: Parser Int
pIntSigned = L.signed sc pInt

-- Parse signed float literals
pFloatSigned :: Parser Double
pFloatSigned = L.signed sc pFloat

-- Parse a keyword
pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme $
  string keyword <* notFollowedBy alphaNumChar

-- Parse a variable expression
pVariable :: Parser Expr
pVariable = Var . T.pack <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

-- Parse an integer expression
pInteger :: Parser Expr
pInteger = Int <$> pInt

-- Parse and expression in parens
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Term parser
pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

-- Expression parser
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

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
