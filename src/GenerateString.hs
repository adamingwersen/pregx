module GenerateString 
        (
            generateString
        )
 where

import StateMachine
import Control.Monad.Random
import Data.Maybe
import Data.Ratio ( (%) )

-- Short binding
(?#) = generateString

-- Introduce new types
type ProbabilitySet = (Char, Float)
type GeneratorSet = (Char, Rational)

-- Generates rep chars from a pregex
generateString pregex rep = do
    generator <- evalRandIO . sequence . repeat . fromList $ getGMap pregex -- evalRandIO requires rationals
    print $ take rep generator

-- Maps a NFA to a generatorset of (Char, Rational)
getGMap pregex = 
    convertToGeneratorSet (convertToProbabilityMap (toNFA pregex))

-- Take a ProbabilitySet and convert [(_, Float)] to [(_, Rational)]
convertToGeneratorSet :: [ProbabilitySet] -> [GeneratorSet]
convertToGeneratorSet  = map (\(a,b) -> (a, readRational (show b)))

-- Make sure transitiontable is not empty
convertToProbabilityMap :: StateMachine -> [ProbabilitySet]
convertToProbabilityMap machine 
    | null (table machine) = error "The NFA has no transitions"
    | otherwise = fillInEmptyProbabilities machine

-- Infer the probabilites of characters that do not have prob attached:
-- "a0.3b" -> [('a', 0.3), ('b', 0.7)]
-- Also infer split probabilities if multiple characters do not have probs:
-- "a0.3bc" -> [('a', 0.3), ('b', 0.35), ('c', 0.35)]
fillInEmptyProbabilities :: StateMachine -> [ProbabilitySet]
fillInEmptyProbabilities machine = 
    let probs = map(\x -> transitionToProbabilitySet x) (table machine)
        suml = sum $ map(\(a,b) -> b) probs
        nzero = length (filter(\x -> snd x == 0.0) probs)
        rest = (1.0 - suml) / fromIntegral nzero
    in spread probs rest

-- Give probability to transitions that do not have them directly
-- Return modified [ProbabilitySet]
spread :: [ProbabilitySet] -> Float -> [ProbabilitySet]
spread probs rest = 
    let x = filter(\x -> snd x == 0.0) probs
    in (map(\(a,b) -> (a, rest)) x) ++ (filter(\x -> snd x > 0.0) probs)

-- Extract needed information out of Transition
-- Return whats needed for calculating probabilites
transitionToProbabilitySet :: Transition -> ProbabilitySet
transitionToProbabilitySet (a,b,c,d) 
    | null d = (fromJust c, 0.0)
    | otherwise = (fromJust c, convertStringToFloat d)

-- Convert string to float
convertStringToFloat :: [Char] -> Float
convertStringToFloat string = read string :: Float

-- Convert string to rational. Needed for evalRandIO
-- https://stackoverflow.com/questions/7056791/how-to-parse-a-decimal-fraction-into-rational-in-haskell
readRational :: String -> Rational
readRational input = read intPart % 1 + read fracPart % (10 ^ length fracPart)
  where (intPart, fromDot) = span (/='.') input
        fracPart           = if null fromDot then "0" else tail fromDot
