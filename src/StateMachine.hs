{-# OPTIONS -XFlexibleContexts  #-}

module StateMachine
    (
        Transition,
        StateMachine(..),
        toNFA
    )  
    where
      
import Data.Set (Set)
import Data.Char (chr, toUpper)
import Data.Maybe (fromJust, isJust)
import Control.Monad.State
import Data.List
import qualified Data.Set as Set


type Node = Int 
type Probability = [Char] 
type TransitionValue = Maybe Char
type Transition = (Node, Node, TransitionValue, Probability)
epsilon = Nothing

-- The Nondeterministic Finite Automata
-- Built from MachineContext
data StateMachine = StateMachine 
                    {
                        table :: [Transition],
                        vocab :: Set Char,
                        startNode :: Node,
                        endNode :: Set Node
                    } deriving(Show, Eq)


-- The State to be passed around 
-- Maintains latest added characters transition node
-- Maintains a list of transitions, that can be modified as characters are parsed
-- Maintains a list of nodes 
-- Maintains a vocabulary of added characters
-- Maintains a temporary operator stash
type ContextOperators = [ Char ]
type StateMachineContext c = State MachineContext c --FiLL 
data MachineContext = Context
                    {
                        contextNodes :: [Node],
                        contextTransitions :: [Transition],
                        contextOperators :: ContextOperators,
                        nextNode :: Node,
                        contextLatest :: Int,
                        contextVocab :: Set Char
                    } deriving (Show, Eq)


-- List of tuples
-- (Operator, Precedence)
type Operators = [(Char, Int)]
operators :: Operators
operators = 
    [
        ( '(', 1 ),
        ( ')', 1 ),
        ( '*', 5 ),
        ( '|', 10 ),
        ( '\NUL', 7 )
    ]
    
eps = chr 2
opFunctions = 
    [
        ( '(', fParenthesis ),
        ( ')', fParenthesis ),
        ( '*', fKleene ),
        ( '|', fChoice ),
        ( '\NUL', fMove )
    ]



-- Allowed set of characters
allowed :: [Char]
allowed = "abcdefghijklmnopqrstuvwxyz()~*|1234567890."

-- Search string for a character
charPresent :: Char -> [Char] -> Bool
charPresent char string
    | null string = False
    | char == toUpper (head string) = True
    | char == (head string) = True
    | otherwise = charPresent char (tail string)

-- Utility
opLookup :: (Maybe Int -> t) -> Char -> t
opLookup f x = f ( lookup x operators )

isAllowed :: Char -> Bool
isAllowed c = charPresent c allowed

isOperator :: Char -> Bool
isOperator = opLookup isJust

isAlph :: Char -> Bool
isAlph c = charPresent c "abcdefghijklmnopqrstuvwxyz"

isNumer :: Char -> Bool
isNumer c = charPresent c "1234567890."

isValue :: Char -> Bool 
isValue c = (isNumer c) || (isAlph c)

getPrecedence :: Char -> Int
getPrecedence = opLookup fromJust

-- Returns a new transition with prob char appended
-- .. if nodes match
incrTransProb :: Int -> Char -> Transition -> Transition
incrTransProb node ch (a,b,c,d)
  | node == b = (a,b,c, d ++ [ch])
  | otherwise = (a,b,c,d)

-- Makes sure, that spacers are introduced between characters. 
-- .. if these are not attached by parentheses or union
preprocess :: [Char] -> [Char]
preprocess (p:m:s)
    | (p == '(' && m == '|') = '(':(preprocess ('|':s))
    | (p == '|' && m == ')') = '|':(preprocess (')':s))
    | ((isValue p || p == ')' || p == '*') && (isValue m || m == '(')) = p:(preprocess (m:s))
    | otherwise = p:preprocess (m:s)
preprocess s = s

-- Constructs a machinecontext from string and passes to an NFA
toNFA str = let context = snd $ parse str
                table = contextTransitions context
                start = last $ contextNodes context
                final = head $ contextNodes context
                latest = contextLatest context
                vocab = contextVocab context
                in StateMachine { table = table,
                                  vocab = vocab,
                                  startNode = start, 
                                  endNode = Set.singleton final
                                }


-- Initialises empty context and traverses a string until empty
parse str = (runState ( do
                mapM_ processChar (preprocess str)
                untilEmpty) (Context [] [] [] 0 0 Set.empty))

-- Handles each char and passes to relevant function
processChar c
  | isAlph c  = processAlpha c
  | isNumer c = processAlpha c
  | isOperator c = error "--> Operators not implemented"
  | otherwise = error ("--> Invalid Input Character: " ++ [c])

-- Handles numbers and letters
processAlpha c = do
    nodeFrom <- addNode
    nodeTo <- addNode
    state <- get
    let isEps = c == eps
        oldTrans = contextTransitions state
        lastAlph = contextLatest state
        getVocab c =  if isEps then epsilon 
                        else Just c
        getVals = if isEps then Set.insert c (contextVocab state) 
                    else contextVocab state
        newNodes = nodeTo:nodeFrom:(contextNodes state)
        getTrans c = if isNumer c then map (\ x -> incrTransProb lastAlph c x) oldTrans
                        else (nodeFrom, nodeTo, getVocab c, "") : oldTrans
    if isNumer c
        then put ( state { contextNodes = newNodes, contextTransitions = getTrans c } )
        else put ( state { contextNodes = newNodes, contextTransitions = getTrans c, contextLatest = head newNodes } )
                         

-- Checks if contextOperators are present. 
-- .. if so, return the latest one
peeks = do
    operators <- gets contextOperators
    if null operators then return $ Nothing else return ( Just (head operators))

-- Traverses operators. 
-- Not used as intentioned.
-- Just passes 
untilEmpty = do
    operator <- peeks
    case operator of
        Just op -> (fromJust $ lookup op opFunctions) >> untilEmpty
        Nothing -> return ()

-- Constructs new Node
-- ..given current StateMachineContext
addNode :: StateMachineContext Node
addNode = do
  state <- get
  put ( state { nextNode = ( nextNode state ) + 1 } )
  return (nextNode state)

--
-- Not implemented
-- 
fParenthesis :: StateMachineContext ()
fParenthesis = do
    state <- get 
    put state
fKleene :: StateMachineContext ()
fKleene = do
    state <- get 
    put state
fChoice :: StateMachineContext ()
fChoice = do
    state <- get 
    put state
fMove :: StateMachineContext ()
fMove = do
    state <- get 
    put state
