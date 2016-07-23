module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.ConstrainedDynamic
    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
         testCase "Hookup test" $ testMe @?= 1
        ]


