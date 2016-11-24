module Lib
    ( someFunc
    , Email(..)
    , emailParser
    ) where

import Text.Parsec

data Email = Email String String
  deriving (Eq, Show)

emailParser :: Parsec String st Email
emailParser = do
        localPart <- many1 $ noneOf "@"
        char '@'
        domainPart <- many1 anyChar
        return $ Email localPart domainPart

someFunc :: IO ()
someFunc = putStrLn "someFunc"
