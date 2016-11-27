module Main where

import Control.Monad

import Lib (emailParser)
import Examples (validEmails, invalidEmails)
import ParserUtil (test)

import HelloWorld (samples)

main :: IO ()
main = do
  samples
  putStrLn "\nvalid emails:"
  test' (validEmails!!1)
  forM_ validEmails test'
  putStrLn "\ninvalid emails:"
  forM_ invalidEmails test'
    where test' = test emailParser $ \ email result ->
                    putStrLn $ show email ++ " => " ++ show result
