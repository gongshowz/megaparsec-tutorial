module Main where

import Text.Megaparsec (parseTest)

import Tutorial.Indent (pItemList, sampleIL)
import Tutorial.Lexing (pExpr, sampleExpr)
import Tutorial.Uri    (pUri)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

separator :: T.Text
separator =
  "\n**********************************************************************************************"

main :: IO ()
main = do
  TIO.putStrLn separator
  TIO.putStrLn sampleIL
  parseTest pItemList sampleIL
  --
  TIO.putStrLn separator
  TIO.putStrLn sampleExpr
  parseTest pExpr sampleExpr
  --
  TIO.putStrLn separator
  putStrLn "\nftp://john:secret@some.host.com:23232"
  parseTest pUri "ftp://john:secret@some.host.com:23232"
  putStrLn "\nhttps://some.host.com/some/path?a=1&b=2&c=3#faq"
  parseTest pUri "https://some.host.com/some/path?a=1&b=2&c=3#faq"
