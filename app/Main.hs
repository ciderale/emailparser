module Main where

import Control.Monad
import Text.Parsec
import Lib
import Examples
import HelloWorld

printAndParse email = do
  putStrLn ""
  putStr "######## input: "
  putStrLn email
  putStr "result: "
  parseTest emailParser email

main :: IO ()
main = do
  samples
  putStrLn "valid emails: ##################################################"
  printAndParse (validEmails!!1)
  forM_ validEmails printAndParse
  putStrLn "invalid emails #################################################"
  forM_ invalidEmails printAndParse
