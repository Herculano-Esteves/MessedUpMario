module Main where

import Test.HUnit
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
test_suite_01 = test [  "Basic Test" ~: True ~=? colisoesParede mapaTeste per1
                    ]











main :: IO ()
main = runTestTTAndExit $ test [test_suite_01]
