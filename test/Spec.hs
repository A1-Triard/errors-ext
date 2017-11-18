#define TESTS
#include <haskell>
import qualified Control.Error.Extensions.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Control.Error.Extensions.Spec.tests
  ]
