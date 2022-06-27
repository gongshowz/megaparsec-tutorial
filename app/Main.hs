module Main where

import Text.Megaparsec

import Tutorial.Indent (pItemList, sample)
import Tutorial.Lexing (pExpr)
import Tutorial.Uri    (pUri)

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  putStrLn "*\n*\n*"
  --
  TIO.putStrLn sample
  parseTest pItemList sample
  --
  putStrLn "*\n*\n*"
  parseTest pExpr "(2 * 3) + 5"
  --
  putStrLn "*\n*\n*"
  putStrLn "https://www.google.com/"
  parseTest pUri "https://www.google.com/"
  putStrLn "\nftp://john:secret@some.host.com:23232"
  parseTest pUri "ftp://john:secret@some.host.com:23232"
  putStrLn "\nhttp://some.host.com/some/path?a=1&b=2&c=3#faq"
  parseTest pUri "http://some.host.com/some/path?a=1&b=2&c=3#faq"
