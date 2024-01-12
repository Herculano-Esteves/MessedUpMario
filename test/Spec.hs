module Main where

import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec
import Tarefa4Spec
import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ test [testesTarefa1, testesTarefa2, testesTarefa3, testesTarefa4]
