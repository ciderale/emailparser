module Lib
    ( someFunc
    , Email(..)
    , emailParser
    ) where

import Text.Parsec

data IP = IPv4 Int Int Int Int
        | IPv6 String
        deriving (Eq, Show)
data Domain = DomainName [String]
            | DomainIP IP
            deriving (Eq, Show)

data Email = Email String Domain
  deriving (Eq, Show)

-- the very basic characters
latin = ['a'..'z'] ++ ['A'..'Z']
digits = ['0'..'9']
specials = "!#$%&'*+-/=?^_`{|}~"
baseChars = oneOf $ latin ++ digits ++ specials

-- characters allowed within quotes
quotedChars = oneOf " (),:;<>@[]."
escapedChars = oneOf "\"\\"

quotedString = char '"' *> stringInQuotes <* char '"'
  where stringInQuotes = mconcat <$> many1 charsInQuotes
        charsInQuotes = many1 (baseChars <|> quotedChars) <|> escapedChars2
        escapedChars2 = (:) <$> char '\\' <*> count 1 escapedChars


-- the local part
localParserPart = quotedString <|> many1 baseChars
localParser = mconcat <$> localParserPart `sepBy1` char '.'

ipParser :: Parsec String st IP
ipParser = v6 <|> v4
    where v6 = string "IPv6:" >> IPv6 <$> many1 (noneOf "]")
          v4 = IPv4 <$> number <* dot <*> number <* dot <*> number <* dot <*> number
          number = read <$> many1 (oneOf digits)
          dot = char '.'

dnsLabel =  many1 $ oneOf ('-' : latin ++ digits)
domainParser = DomainIP <$> between (char '[') (char ']') ipParser
           <|> DomainName <$> dnsLabel `sepBy1` char '.'


emailParser :: Parsec String st Email
emailParser = do
        localPart <- localParser
        char '@'
        domainPart <- domainParser
        return $ Email localPart domainPart

someFunc :: IO ()
someFunc = putStrLn "someFunc"
