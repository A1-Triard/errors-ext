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

-- | This module exports bracket-like functions for 'ExceptT'.
--

module Control.Error.Extensions
  ( bracketE
  , bracketE_
  ) where

import Control.Error.Util
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- | Analogous to 'Control.Exception.bracket', but for 'ExceptT' over 'IO'
-- (or 'ExceptT' over any 'Control.Monad.Catch.MonadMask' monad).
bracketE :: MonadMask m => ExceptT e m a -> (a -> ExceptT e m b) -> (a -> ExceptT e m c) -> ExceptT e m c
bracketE acquire release action = (hoistEither =<<) $ lift $ do
  resource <- runExceptT acquire
  result <- bracketOnError (return resource) (ignoreAll . ioRelease) ioAction
  if isLeft result
    then ignoreAll (ioRelease resource) >> return result
    else caseResult result <$> ioRelease resource
  where
    ignoreAll = handleAll (const $ return ()) . void
    ioAction (Left e) = return $ Left e
    ioAction (Right r) = runExceptT $ action r
    ioRelease (Left e) = return $ Left e
    ioRelease (Right r) = runExceptT $ release r
    caseResult (Left e) _ = Left e
    caseResult (Right _) (Left e) = Left e
    caseResult (Right r) (Right _) = Right r
{-# INLINE bracketE #-}

-- | A variant of 'bracketE' where the return value from the first computation is not required.
bracketE_ :: MonadMask m => ExceptT e m a -> ExceptT e m b -> ExceptT e m c -> ExceptT e m c
bracketE_ acquire release action = bracketE acquire (const release) (const action)
{-# INLINE bracketE_ #-}
