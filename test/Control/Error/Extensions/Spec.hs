module Control.Error.Extensions.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Control.Error.Extensions

tests :: Test
tests = TestList $ (TestCase . runBracketTest) <$> bracketTests

data Resource = Resource { acquired :: !Bool, processed :: !Bool, released :: !Bool } deriving (Eq, Show)

resource :: IO (IORef Resource)
resource = newIORef $ Resource False False False

type Acquire = IORef Resource -> ExceptT String IO (IORef Resource)

acquireOk :: Acquire
acquireOk r = do
  lift $ modifyIORef' r $ \x -> x { acquired = True }
  return r

acquireEx :: Acquire
acquireEx _ = lift $ fail "acquireEx"

acquireEr :: Acquire
acquireEr _ = throwE "acquireEr"

type Release = IORef Resource -> ExceptT String IO ()

releaseOk :: Release
releaseOk r = lift $ modifyIORef' r $ \x -> x { released = True }

type Process = IORef Resource -> ExceptT String IO Resource

processOk :: Process
processOk r = lift $ do
  v <- readIORef r
  modifyIORef' r $ \x -> x { processed = True }
  return v

data Error = Error { isException :: !Bool, message :: !String } deriving (Eq, Show)

bracketTest :: Acquire -> Release -> Process -> IO (Resource, Either Error Resource)
bracketTest acquire release process = do
  r <- resource
  p <- catch (either (\x -> Left $ Error False x) Right <$> (runExceptT $ bracketE (acquire r) release process)) $ \e ->
    let _ = e :: IOError in return $ Left $ Error True $ ioeGetErrorString e
  f <- readIORef r
  return (f, p)

data TestData = TestData String !Acquire !Release !Process !(Resource, Either Error Resource)

bracketTests :: [TestData]
bracketTests =
  [ TestData "1" acquireOk releaseOk processOk (Resource True True True, Right $ Resource True False False)
  , TestData "2" acquireEx releaseOk processOk (Resource False False False, Left $ Error { isException = True, message = "acquireEx" })
  , TestData "3" acquireEr releaseOk processOk (Resource False False False, Left $ Error { isException = False, message = "acquireEr" })
  ]

runBracketTest :: TestData -> Assertion
runBracketTest (TestData m a r p e) = do
  t <- bracketTest a r p
  assertEqual m e t
