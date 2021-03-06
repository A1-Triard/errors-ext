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
