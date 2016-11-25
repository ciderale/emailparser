import           Data.Text

import           Text.Parsec

import           Data.Either
import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib
import           Examples

main = defaultMain tests

tests = testGroup "Email Validation"
    [ testGroup "valid addresses" $ assert isRight <$> validEmails
    , testGroup "invalid addresses" $ assert (not.isRight) <$> invalidEmails
    ]
    where
      assert pred email = testCase email $ pred result @?= True
        where result = parse emailParser "" email
