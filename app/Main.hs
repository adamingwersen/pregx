module Main where

import StateMachine
import GenerateString (generateString)

--example use of generateString
main :: IO ()
main = generateString "a0.3b0.1b0.1c" 100
