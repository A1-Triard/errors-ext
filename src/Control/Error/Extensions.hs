--
-- Copyright 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

-- | This module exports 'bracket'-like functions for @'Control.Monad.Trans.Except.ExceptT' e 'IO'@.

module Control.Error.Extensions
  ( bracketE
  , bracketE_
  ) where

import Control.Exception
import Control.Error.Util
import Control.Monad
import Control.Monad.Catch (handleAll)
import Control.Monad.Error.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except ()

liftedBracketOnError :: MonadBaseControl IO m => m a -> (a -> m b) -> (a -> m c) -> m c
liftedBracketOnError acquire release action = control $ \run ->
  bracketOnError (run acquire) (\saved -> run (restoreM saved >>= release)) (\saved -> run (restoreM saved >>= action))

liftedHandleAll :: MonadBaseControl IO m => (SomeException -> m a) -> m a -> m a
liftedHandleAll handler action = control $ \run ->
  handleAll (run . handler) (run action)

runErrorM :: MonadError e m => m a -> m (Either e a)
runErrorM a = catchError (Right <$> a) (return . Left)

errorM :: MonadError e m => m (Either e a) -> m a
errorM = (either throwError return =<<)

-- | Analogous to 'bracket', but for @'Control.Monad.Trans.Except.ExceptT' e 'IO'@
-- (or any 'MonadError' allowing 'bracket' lifting).
bracketE :: (MonadBaseControl IO m, MonadError e m) => m a -> (a -> m b) -> (a -> m c) -> m c
bracketE acquire release action = errorM $ do
  resource <- runErrorM acquire
  result <- liftedBracketOnError (return resource) (ignoreAll . ioRelease) ioAction
  if isLeft result
    then ignoreAll (ioRelease resource) >> return result
    else caseResult result <$> ioRelease resource
  where
    ignoreAll = liftedHandleAll (const $ return ()) . void
    ioAction (Left e) = return $ Left e
    ioAction (Right r) = runErrorM $ action r
    ioRelease (Left e) = return $ Left e
    ioRelease (Right r) = runErrorM $ release r
    caseResult (Left e) _ = Left e
    caseResult (Right _) (Left e) = Left e
    caseResult (Right r) (Right _) = Right r
{-# INLINE bracketE #-}

-- | A variant of 'bracketE' where the return value from the first computation is not required.
bracketE_ :: (MonadBaseControl IO m, MonadError e m) => m a -> m b -> m c -> m c
bracketE_ acquire release action = bracketE acquire (const release) (const action)
{-# INLINE bracketE_ #-}
