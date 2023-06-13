module Test.Main where

import Prelude

import Data.Array

import Effect (Effect)
import Effect.Console (log)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.QuickCheck as QC

import Core


testification :: TestSuite
testification =
    suite "Testification" do
        test "sanity" do
           Assert.assert "the world has gone mad" true

propSetWeight :: Exercise -> Int -> Boolean
propSetWeight exercise x = x == result where
    result = (\(Exercise record) -> record.weight) <<< setWeight exercise $ x

propSetSuccess :: Exercise -> Boolean -> Boolean
propSetSuccess exercise x = x == result where
    result = (\(Exercise record) -> record.succeed) <<< setSuccess exercise $ x

main :: Effect Unit
main = do
    runTest do
        testification
    QC.quickCheck' 10 propSetWeight
    QC.quickCheck' 2 propSetSuccess
