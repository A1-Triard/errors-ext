module Control.Error.Extensions
  ( bracketE
  , bracketE_
  ) where

import Control.Error.Util
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- | Analogous to 'bracket', but for ExceptT over IO.
bracketE :: ExceptT e IO a -> (a -> ExceptT e IO b) -> (a -> ExceptT e IO c) -> ExceptT e IO c
bracketE acquire release action = (hoistEither =<<) $ lift $ do
  resource <- runExceptT acquire
  result <- bracketOnError (return resource) (handleAll . ioRelease) ioAction
  if isLeft result
    then handleAll (ioRelease resource) >> return result
    else caseResult result <$> ioRelease resource
  where
    handleAll = handle (\x -> let _ = x :: SomeException in return ()) . void
    ioAction (Left e) = return $ Left e
    ioAction (Right r) = runExceptT $ action r
    ioRelease (Left e) = return $ Left e
    ioRelease (Right r) = runExceptT $ release r
    caseResult (Left e) _ = Left e
    caseResult (Right _) (Left e) = Left e
    caseResult (Right r) (Right _) = Right r

-- | A variant of 'bracketE' where the return value from the first computation is not required.
bracketE_ :: ExceptT e IO a -> ExceptT e IO b -> ExceptT e IO c -> ExceptT e IO c
bracketE_ acquire release action = bracketE acquire (const release) (const action)
