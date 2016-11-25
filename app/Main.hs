module Main where

import Control.Monad
import Text.Parsec
import Lib
import Examples

printAndParse email = do
  putStrLn ""
  putStr "######## input: "
  putStrLn email
  putStr "result: "
  parseTest emailParser email

main :: IO ()
main = do
  putStrLn "valid emails: ##################################################"
  printAndParse (validEmails!!1)
  -- printAndParse (validEmails!!2)
  -- printAndParse (validEmails!!3)
  forM_ validEmails printAndParse
  putStrLn "invalid emails #################################################"
  forM_ invalidEmails printAndParse
  --printAndParse (invalidEmails!!1)
