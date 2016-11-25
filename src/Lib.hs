module Lib
    ( Email(..), Domain(..), IP(..)
    , emailParser
    ) where

import Control.Monad
import Text.Parsec

data Email = Email String Domain
  deriving (Eq, Show)

data Domain = DomainName [String]
            | DomainIP IP
            deriving (Eq, Show)

data IP = IPv4 Int Int Int Int
        | IPv6 String
        deriving (Eq, Show)

emailParser :: Parsec String st Email
emailParser = do
        localPart <- ignoreComment localParser
        char '@'
        domainPart <- ignoreComment domainParser
        eof
        return $ Email localPart domainPart

-- the local part & domain part
localParser = mconcat <$> localParserPart `sepBy1` char '.'
localParserPart = (quotedString <|> many1 baseChars) `limitedTo` 64

domainParser = DomainIP <$> between2 "[]" ipParser
           <|> DomainName <$> dnsLabel `sepBy1` char '.'

ipParser :: Parsec String st IP
ipParser = v6 <|> v4
    where v6 = string "IPv6:" >> IPv6 <$> many1 (noneOf "]")
          v4 = IPv4 <$> number <* dot <*> number <* dot <*> number <* dot <*> number
          number = read <$> many1 (oneOf digits)
          dot = char '.'

dnsLabel = many1 dnsLabelChar `limitedTo` 63
  where dnsLabelChar = oneOf $ '-' : latin ++ digits

-- utilities for length limitation & comment handling
ignoreComment = between (optional comment) (optional comment)
  where comment = between2 "()" (many1 (noneOf ")"))

p `limitedTo` len = do str <- p
                       guard $ length str <= len
                       return str

between2 :: String -> Parsec String st a -> Parsec String st a
between2 [open,close] = between (char open) (char close)

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
