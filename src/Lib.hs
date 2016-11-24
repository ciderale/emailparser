module Lib
    ( someFunc
    , Email(..)
    , emailParser
    ) where

import Text.Parsec

data Email = Email String [String]
  deriving (Eq, Show)

dnsLabel =  many1 $ noneOf "."
domainParser = dnsLabel `sepBy1` char '.'

emailParser :: Parsec String st Email
emailParser = do
        localPart <- many1 $ noneOf "@"
        char '@'
        domainPart <- domainParser
        return $ Email localPart domainPart

someFunc :: IO ()
someFunc = putStrLn "someFunc"
