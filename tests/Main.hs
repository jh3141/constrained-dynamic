module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.ConstrainedDynamic
import Data.Typeable
    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
         testCase "Can convert a string to ConstrainedDynamic and back" $
                  fromDynamic (toDyn "hello") @?= Just "hello",
         testCase "Can convert a bool to ConstrainedDynamic and back" $
                  fromDynamic (toDyn True) @?= Just True,
         testCase "Cannot convert string to CD and back to a bool" $
                  fromDynamic (toDyn "hello") @?= (Nothing::Maybe Bool)
        ]
