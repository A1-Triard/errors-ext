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

-- | This module exports bracket-like functions for 'ExceptT' over 'IO' monad.
--

module Control.Error.Extensions
  ( bracketE
  , bracketE_
  ) where

import Control.Error.Util
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- | Analogous to 'bracket', but for 'ExceptT' over 'IO'.
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
