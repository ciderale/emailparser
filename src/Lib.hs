module Lib
    ( Email(..), Domain(..), IP(..)
    , emailParser
    ) where

import Control.Monad
import Data.List
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
localParser = intercalate "." <$> localParserPart `sepBy1` dot
localParserPart = (quotedString <|> manyOf1 baseChars) `limitedTo` 64

domainParser = DomainIP <$> between2 "[]" ipParser
           <|> DomainName <$> dnsLabel `sepBy1` dot

ipParser :: Parsec String st IP
ipParser = v6 <|> v4
    where v6 = string "IPv6:" >> IPv6 <$> many1 (noneOf "]")
          v4 = IPv4 <$> number <* dot <*> number <* dot <*> number <* dot <*> number
          number = read <$> manyOf1 digits

dnsLabel = manyOf1 dnsLabelChar `limitedTo` 63
  where dnsLabelChar = '-' : latin ++ digits

-- some handy combinators
manyOf1 = many1 . oneOf

ignoreComment = between (optional comment) (optional comment)
  where comment = between2 "()" (many1 (noneOf ")"))

p `limitedTo` len = do str <- p
                       guard $ length str <= len
                       return str

between2 :: String -> Parsec String st a -> Parsec String st a
between2 [open,close] = between (char open) (char close)

-- the very basic characters
dot = char '.'
latin = ['a'..'z'] ++ ['A'..'Z']
digits = ['0'..'9']
specials = "!#$%&'*+-/=?^_`{|}~"
baseChars = latin ++ digits ++ specials

-- characters allowed within quotes
quotedChars = " (),:;<>@[]."
escapedChars = "\"\\"

quotedString = char '"' *> stringInQuotes <* char '"'
  where stringInQuotes = mconcat <$> many1 charsInQuotes
        charsInQuotes = manyOf1 (baseChars ++ quotedChars) <|> escapedChars2
        escapedChars2 = (:) <$> char '\\' <*> count 1 (oneOf escapedChars)
