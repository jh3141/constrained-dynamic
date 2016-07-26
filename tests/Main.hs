module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.ConstrainedDynamic
import Data.Typeable

main :: IO ()
main = defaultMain tests

type CDShow = ConstrainedDynamic Show
type CDEq = ConstrainedDynamic Eq

tests :: TestTree
tests = testGroup "Tests" [
         testCase "Can convert a string to ConstrainedDynamic and back" $
                  fromDynamic (toDyn "hello" :: CDShow) @?= Just "hello",
         testCase "Can convert a bool to ConstrainedDynamic and back" $
                  fromDynamic (toDyn True :: CDShow) @?= Just True,
         testCase "Cannot convert string to CD and back to a bool" $
                  fromDynamic (toDyn "hello" :: CDShow) @?= (Nothing::Maybe Bool),
         testCase "Apply a function from a typeclass to a CD value" $
                  (toDyn True :: CDShow) `applyClassFn` show @?= "True",
         testCase "Valid classCast returns Just" $
                  maybe "cast failed" (const "cast OK")
                            (classCast (toDyn "hello" :: CDShow)
                             :: Maybe CDShow) @?= "cast OK",
         testCase "Invalid classCast returns Nothing" $
                  maybe "cast failed" (const "cast OK")
                             (classCast (toDyn "hello" ::CDShow)
                              :: Maybe CDEq) @?= "cast failed",
         testCase "Showing a 'ConstrainedDynamic Show' shows value" $
                  show (toDyn "hello" :: CDShow) @?= "\"hello\"",
         testCase "Showing a 'ConstrainedDynamic Eq' shows type" $
                  show (toDyn "hello" :: CDEq) @?= "[Char]"
        ]
