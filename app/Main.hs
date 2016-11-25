module Main where

import Text.Parsec
import Lib

main :: IO ()
main = do
  parseTest emailParser  "hallo.\"wo\".go@[123.99.33.44]"
  parseTest emailParser  "hallo.\"wo\".go@[123.ii.33.44]"
  parseTest emailParser  "asdf@wo.com"
  parseTest emailParser  "asdf@wo1234567890123456789012345678901234567890123456789012345678901234567890.com"
