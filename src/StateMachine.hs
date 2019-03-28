{-# OPTIONS -XFlexibleContexts  #-}

module StateMachine where
      
import Data.Char
import Control.Monad.State
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


data Reg = Eps
    | Sym Char
    | Num Double
    | Choice Reg Reg
    | Kleene Reg
    | Braces Reg
    | Parentheses Reg
    deriving (Show, Eq)

--FILL
--FILL
--FILL
type Node = Integer --FILL
type Probability = (Char, Double) --FILL
type TransitionValue = Maybe Char --FILL
type Transition = ( Node, Node, TransitionValue, Probability )

data StateMachine = StateMachine 
                    {
                        table :: [Transition],
                        probas :: [Probability],
                        vocab :: Set Char,
                        startNode :: Node,
                        endNode :: Set Node
                    } deriving(Show, Eq)

--FILL
--FILL
type ContextOperators = [ Char ]
type StateMachineContext c = State MachineContext c --FiLL 
data MachineContext = Context
                    {
                        contextNodes :: [Node],
                        contextTransitions :: [Transition],
                        contextProbas :: [Probability],
                        contextOperators :: ContextOperators,
                        nextNode :: Node,
                        values :: Set Char
                    } deriving (Show, Eq)


--FILL
--FILL
type Operators = [ ( Char, Int ) ] --FILL
operators :: Operators
operators = 
    [
        ( '(', 1 ),
        ( ')', 1 ),
        ( '~', 2 ),
        ( '*', 5 ),
        ( '|', 10 ),
        ( '\NUL', 7 )
    ]

oFunctions = 
    [
        ( '(', fParenthesis ),
        ( ')', fParenthesis ),
        ( '~', fTilda ),
        ( '*', fKleene ),
        ( '|', fChoice ),
        ( '\NUL', fMove )
    ]

--
-- Utilities
--

allowed :: [Char]
allowed = "abcdefghijklmnopqrstuvwxyz()~*|1234567890."

charPresent :: Char -> [Char] -> Bool
charPresent char string
    | null string = False
    | char == toUpper (head string) = True
    | char == (head string) = True
    | otherwise = charPresent char (tail string)

oLookup :: (Maybe Int -> t) -> Char -> t
oLookup f x = f ( lookup x operators )

isAllowed :: Char -> Bool
isAllowed c = charPresent c allowed

isOperator :: Char -> Bool
isOperator = oLookup isJust

isAlph :: Char -> Bool
isAlph c = charPresent c "abcdefghijklmnopqrstuvwxyz"

isNumer :: Char -> Bool
isNumer c = charPresent c "1234567890."

getPrecedence :: Char -> Int
getPrecedence = oLookup fromJust

getoFunction :: Char -> StateMachineContext ()
getoFunction c = fromJust ( lookup c oFunctions)


--
-- Main Logic
-- 

processChar c 
    | (isAlph c) or (isNumer c) = processInput c
    | isOperator c = processOperator c

processInput c =
    if isNumer c 
        then do
            state <- get
            let transitions = contextTransitions state









--
-- StateMachineContext Operator Functions
--

addNode :: StateMachineContext Node
addNode = do
  state <- get
  put ( state { nextNode = ( nextNode state ) + 1 } )
  return (nextNode state)

fParenthesis :: StateMachineContext ()
fParenthesis = do
    st <- get 
    put st
fTilda :: StateMachineContext ()
fTilda = do
    st <- get 
    put st
fKleene :: StateMachineContext ()
fKleene = do
    st <- get 
    put st
fChoice :: StateMachineContext ()
fChoice = do
    st <- get 
    put st
fMove :: StateMachineContext ()
fMove = do
    st <- get 
    put st









