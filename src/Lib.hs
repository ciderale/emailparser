module Lib
    ( someFunc
    , Email(..)
    , emailParser
    ) where

import Text.Parsec

data Email = Email String [String]
  deriving (Eq, Show)

latin = ['a'..'z'] ++ ['A'..'Z']
digits = ['0'..'9']
specials = "!#$%&'*+-/=?^_`{|}~"
baseChars = oneOf $ latin ++ digits ++ specials

dnsLabel =  many1 $ noneOf "."
domainParser = dnsLabel `sepBy1` char '.'

localParser = many1 baseChars


emailParser :: Parsec String st Email
emailParser = do
        localPart <- localParser
        char '@'
        domainPart <- domainParser
        return $ Email localPart domainPart

someFunc :: IO ()
someFunc = putStrLn "someFunc"
