errors-ext
==========

[![Hackage version](https://img.shields.io/hackage/v/errors-ext.svg)](https://hackage.haskell.org/package/errors-ext)
[![Stackage version](https://www.stackage.org/package/errors-ext/badge/lts?label=stackage)](https://www.stackage.org/package/errors-ext)
[![Stackage nightly version](https://www.stackage.org/package/errors-ext/badge/nightly?label=nightly)](https://www.stackage.org/package/errors-ext)
[![Build status](https://secure.travis-ci.org/A1-Triard/errors-ext.png?branch=master)](http://travis-ci.org/A1-Triard/errors-ext)

`bracket`-like functions for `ExceptT e IO` monad.

The following example demonstartes a typical use-case. It is a program which reads a file as a sequence of natural numbers,
and produces a new file with every number replaced by its natural predecessor. The program deals with two kinds of errors:
I/O errors, and out-of-valid-range value in input sequence.

```haskell
{- errors-ext-example.hs -}
{- Compile: ghc errors-ext-example.hs \
                -package binary-ext \
                -package bytestring \
                -package conduit \
                -package errors \
                -package errors-ext \
                -package monad-loops \
                -package mtl
-}

{-# LANGUAGE BangPatterns #-}

import Control.Exception
import Control.Error.Extensions
import Control.Monad.Error.Class
import Control.Monad.Loops
import Control.Monad.Trans.Except
import Control.Error.Util hiding (err, errLn)
import Data.Conduit
import qualified Data.Conduit.Combinators as N
import Data.Conduit.Parsers.Binary.Get
import Data.Conduit.Parsers.Binary.Put
import qualified Data.ByteString as S (ByteString)
import Data.Maybe hiding (fromJust)
import System.Exit
import System.IO
import System.IO.Error

data PrevNaturalError = NotNatural Integer | Minimal deriving (Eq, Show)

prevNatural :: (Monad m, Integral i) => ConduitT i i (ExceptT PrevNaturalError m) ()
prevNatural = awaitForever $ \ !n -> do
  if n <= 0 then throwError (NotNatural $ fromIntegral n) else return ()
  if n == 1 then throwError Minimal else return ()
  yield $ n - 1

prevNaturalsFile :: Monad m => ConduitT S.ByteString S.ByteString (ExceptT PrevNaturalError m) ()
prevNaturalsFile = (eitherVoidR <$> runGet (iterateM_ (const $ yield =<< getInt8) (error ""))) .| prevNatural .| awaitForever (runPut . putInt8)

main :: IO ()
main = do
  err <- printErrors getErrorText run
  if err then exitFailure else exitSuccess

printErrors :: (e -> String) -> ExceptT e IO () -> IO Bool
printErrors error_text action = do
  result <- runExceptT action
  case result of
    Left e -> do
      handle (\x -> let _ = x :: IOError in return ()) $ hPutStrLn stderr $ error_text e
      return True
    Right _ ->
      return False

getErrorText :: IOError -> String
getErrorText e
  | isUserError e = ioeGetErrorString e
  | isDoesNotExistError e = fromMaybe "" (ioeGetFileName e) ++ ": No such file or directory."
  | otherwise = ioeGetErrorString e

prevNaturalErrorText :: PrevNaturalError -> String
prevNaturalErrorText (NotNatural n) = "The `" ++ show n ++ "' number is not natural."
prevNaturalErrorText Minimal = "The minimal natural number does not have a predecessor."

run :: ExceptT IOError IO ()
run =
  bracketE (tryIO $ openFile "input" ReadMode) (tryIO . hClose) $ \input ->
    bracketE (tryIO $ openFile "output" WriteMode) (tryIO . hClose) $ \output ->
      mapError (userError . prevNaturalErrorText) $ runConduit $ N.sourceHandle input .| prevNaturalsFile .| N.sinkHandle output
```
