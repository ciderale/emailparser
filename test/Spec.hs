{-# LANGUAGE OverloadedStrings #-}
import           Data.Text

import Text.Parsec

import           Data.Either
import           Test.Tasty
import           Test.Tasty.HUnit

import Lib

main = defaultMain tests

tests = testGroup "Email Validation"
    [ testGroup "valid addresses" $ test True <$> validEmails
    , testGroup "invalid addresses" $ test False <$> invalidEmails
    ]
    where
      validate = isRight . parse emailParser ""

      test correct email = testCase email $ validate email @?= correct

      validEmails =
        [ "prettyandsimple@example.com"
        , "very.common@example.com"
        , "disposable.style.email.with+symbol@example.com"
        , "other.email-with-dash@example.com"
        , "x@example.com"
        , "\"much.more unusual\"@example.com"
        , "\"very.unusual.@.unusual.com\"@example.com"
        , "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"
        , "example-indeed@strange-example.com"
        , "admin@mailserver1"
        , "#!$%&'*+-/=?^_`{}|~@example.org"
        , "\"()<>[]:,;@\\\\\\\"!#$%&'-/=?^_`{}| ~.a\"@example.org"
        , "\" \"@example.org"
        , "example@localhost"
        , "example@s.solutions"
        , "user@localserver"
        , "user@tt"
        , "user@[IPv6:2001:DB8::1]"
        , "jsmith@[192.168.2.1]"
        ]
      invalidEmails =
        [ "Abc.example.com" -- (no @ character)
        , "A@b@c@example.com" -- (only one @ is allowed outside quotation marks)
        , "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com" -- (none of the special characters in this local-part are allowed outside quotation marks)
        , "just\"not\"right@example.com" -- (quoted strings must be dot separated or the only element making up the local-part)
        , "this is\"not\\allowed@example.com" -- (spaces, quotes, and backslashes may only exist when within quoted strings and preceded by a backslash)
        , "this\\ still\\\"not\\\\allowed@example.com" --  (even if escaped (preceded by a backslash), spaces, quotes, and backslashes must still be contained by quotes)
        , "1234567890123456789012345678901234567890123456789012345678901234+x@example.com" --  (too long)
        , "john..doe@example.com" -- (double dot before @) with caveat: Gmail lets this through, Email address#Local-part the dots altogether
        , "john.doe@example..com"
        ]
