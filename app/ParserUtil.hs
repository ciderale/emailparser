{-# LANGUAGE RankNTypes #-}

module ParserUtil
  (test) where

import Text.Parsec
import Lib

test :: Parser a -> (String -> a -> IO ()) -> String -> IO ()
test parser onSuccess input = do
  putStrLn $ take 80 $ cycle "#"
  let parseResult = parse parser input input
  case parseResult of
    Right value
      -> onSuccess input value
    Left error
      -> putStrLn $ "failed to parse: " ++ show error
