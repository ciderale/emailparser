{-# LANGUAGE Rank2Types #-}

module HelloWorld
  ( hello, word, world
  , helloWorld
  , doit
  , samples
  ) where

import Text.Parsec

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
doit input = do
  putStrLn $ take 80 $ cycle "#"
  let parseResult = parse helloWorld input input
  case parseResult of
    Right description
      -> putStrLn $ "The world's description in " ++ show input
                 ++ " is: " ++ show description
    Left error
      -> putStrLn $ "failed to parse: " ++ show error

samples :: IO ()
samples = do
  doit "hello world"
  doit "hello new world."
  doit "hello brave new world!"
  doit "hello brave-new world!"
  doit "hello braver world!!!"
  doit "hello another world..."
