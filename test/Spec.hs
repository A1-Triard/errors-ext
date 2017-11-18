import Test.HUnit.Base hiding (Label)
import Test.HUnit.Text
import Control.Monad
import qualified Control.Error.Extensions.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Control.Error.Extensions.Spec.tests
  ]
