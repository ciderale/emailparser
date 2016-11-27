{-# LANGUAGE Rank2Types #-}

module HelloWorld
  ( hello, word, world
  , helloWorld
  , samples
  ) where

import Text.Parsec
import ParserUtil

type Parser a = forall state . Parsec String state a

hello,word, world :: Parser String
hello = string "hello"
world = string "world"
word = do
    value <- many1 $ noneOf " "
    char ' '
    return value

helloWorld :: Parser String
helloWorld = do
  hello;
  char ' '
  name <- word
  world
  string "." <|> many (char '!')
  eof
  return name

doit :: String -> IO ()
doit = test helloWorld $ \ input description ->
  putStrLn $ "The world's description in " ++ show input
          ++ " is: " ++ show description

samples :: IO ()
samples = do
  doit "hello world"
  doit "hello new world."
  doit "hello brave new world!"
  doit "hello brave-new world!"
  doit "hello braver world!!!"
  doit "hello another world..."
