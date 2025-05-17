{-
  CS 3304 Project #2
  Author: Ishita Gupta

  Description:
    A complete Haskell Forth interpreter (subset) that:
     - Reads lines from an input file (each line = a Forth program).
     - Interprets each line.
     - Writes output to an output file, one line per input line.
     - Uses purely functional recursion (no loops) and obeys the assignment specs.

  How to compile/run on a typical UNIX-like system:
    ghc -o project_2 main.hs
    ./project_2 input_1.txt myoutput_1.txt
-}

module Main where

import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Data.Char (isDigit)
import Data.List (intercalate)


----------------------------------------------------------------
-- 1) TYPE DEFINITIONS
----------------------------------------------------------------

-- We'll represent the stack as a list of Int,
-- where the *head* is the TOP of the stack.
type Stack = [Int]

-- For our interpreter's result of processing one line:
--   Either "illegal"  if some error occurred
--   or Right (finalStack, itemsPrinted)
type ForthResult = Either String (Stack, [Int])

----------------------------------------------------------------
-- 2) MAIN
----------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let inputFile  = head args
      outputFile = args !! 1

  contents <- readFile inputFile
  let inputLines = lines contents

  -- interpret each line
  let results = map interpretLine inputLines

  -- format and write results
  let outputLines = map formatOutput results
  writeFile outputFile (unlines outputLines ++ "\n")

----------------------------------------------------------------
-- 3) INTERPRET A SINGLE LINE
----------------------------------------------------------------

-- interpretLine:
--   - Takes one line of Forth code as a String.
--   - Splits it into tokens, processes them.
--   - If "illegal", return ("illegal","illegal").
--   - Otherwise return (printedOutputString, stackRemainingString).
interpretLine :: String -> (String, String)
interpretLine line =
  case processTokens (words line) [] [] of
    Left _ -> ("illegal", "illegal")

    Right (finalStack, printedItems) ->
      -- Convert printed items to a string, with spaces:
      let printedStr =
            if null printedItems
               then ""
               else unwords (map show printedItems)
          -- We want the topmost element on the *right*,
          -- but our internal representation has the top at the *head*.
          -- So we reverse finalStack when printing:
          stackStr  =
            if null finalStack
               then ""
               else unwords (map show (reverse finalStack))
      in (printedStr, stackStr)

----------------------------------------------------------------
-- 4) PROCESS TOKENS (CORE INTERPRETER LOGIC)
----------------------------------------------------------------

-- processTokens:
--   - Recursively processes each token
--   - Maintains the current stack, plus the list of printed items so far
--   - On success, returns Right (finalStack, printedItems)
--   - On error, returns Left "illegal"
processTokens :: [String] -> Stack -> [Int] -> ForthResult
processTokens [] stk printedSoFar =
  Right (stk, printedSoFar)

processTokens (tok:rest) stk printedSoFar
  | isNumericToken tok =
    -- If the token looks like a valid Int, parse it and push on stack:
    let val = read tok :: Int
    in processTokens rest (val : stk) printedSoFar

  | otherwise =
    -- It's not numeric, so we try it as a Forth word:
    case tok of
      "."   -> doDot rest stk printedSoFar
      ".S"  -> doDotS rest stk printedSoFar
      "+"   -> doPlus rest stk printedSoFar
      "-"   -> doMinus rest stk printedSoFar
      "*"   -> doStar rest stk printedSoFar
      "/"   -> doSlash rest stk printedSoFar
      "DUP" -> doDup rest stk printedSoFar
      "SWAP"-> doSwap rest stk printedSoFar
      "DROP"-> doDrop rest stk printedSoFar
      "OVER"-> doOver rest stk printedSoFar
      "ROT" -> doRot rest stk printedSoFar
      _     -> Left "illegal"  -- unknown word => error

----------------------------------------------------------------
-- 5) IMPLEMENTATION OF EACH FORTH COMMAND
----------------------------------------------------------------

-- 5.1) Dot (.) => pop top, print it
doDot :: [String] -> Stack -> [Int] -> ForthResult
doDot rest stk printed =
  case stk of
    []     -> Left "illegal"
    (x:xs) -> processTokens rest xs (printed ++ [x])

-- 5.2) DotS (.S) => print entire stack, but do NOT modify it
--      The topmost element of the stack is printed rightmost,
--      so we append the *reverse* of stk to "printed".
doDotS :: [String] -> Stack -> [Int] -> ForthResult
doDotS rest stk printed =
  let newPrinted = printed ++ reverse stk
  in processTokens rest stk newPrinted

-- 5.3) + => pop top two, push sum
doPlus :: [String] -> Stack -> [Int] -> ForthResult
doPlus rest stk printed =
  case stk of
    (x1:x2:xs) -> processTokens rest ((x2 + x1) : xs) printed
    _          -> Left "illegal"

-- 5.4) - => pop top two, push (2nd - top)
doMinus :: [String] -> Stack -> [Int] -> ForthResult
doMinus rest stk printed =
  case stk of
    (x1:x2:xs) -> processTokens rest ((x2 - x1) : xs) printed
    _          -> Left "illegal"

-- 5.5) * => pop top two, push product
doStar :: [String] -> Stack -> [Int] -> ForthResult
doStar rest stk printed =
  case stk of
    (x1:x2:xs) -> processTokens rest ((x2 * x1) : xs) printed
    _          -> Left "illegal"

-- 5.6) / => pop top two, push (2nd / top) (integer div).
--           if top is 0 => illegal
doSlash :: [String] -> Stack -> [Int] -> ForthResult
doSlash rest stk printed =
  case stk of
    (0:x2:xs) -> Left "illegal"  -- division by zero
    (x1:x2:xs) -> processTokens rest ((x2 `div` x1) : xs) printed
    _          -> Left "illegal"

-- 5.7) DUP => duplicate top
doDup :: [String] -> Stack -> [Int] -> ForthResult
doDup rest stk printed =
  case stk of
    []     -> Left "illegal"
    (x:xs) -> processTokens rest (x : x : xs) printed

-- 5.8) SWAP => swap top two
doSwap :: [String] -> Stack -> [Int] -> ForthResult
doSwap rest stk printed =
  case stk of
    (x1:x2:xs) -> processTokens rest (x2:x1:xs) printed
    _          -> Left "illegal"

-- 5.9) DROP => discard top
doDrop :: [String] -> Stack -> [Int] -> ForthResult
doDrop rest stk printed =
  case stk of
    []     -> Left "illegal"
    (_:xs) -> processTokens rest xs printed

-- 5.10) OVER => copy 2nd item and push on top
doOver :: [String] -> Stack -> [Int] -> ForthResult
doOver rest stk printed =
  case stk of
    (x1:x2:xs) -> processTokens rest (x2:x1:x2:xs) printed
    _          -> Left "illegal"

-- 5.11) ROT => rotate top 3: (x1:x2:x3:xs) => (x3:x1:x2:xs)
doRot :: [String] -> Stack -> [Int] -> ForthResult
doRot rest stk printed =
  case stk of
    (x1:x2:x3:xs) -> processTokens rest (x3:x1:x2:xs) printed
    _             -> Left "illegal"

----------------------------------------------------------------
-- 6) HELPER FUNCTIONS
----------------------------------------------------------------

-- isNumericToken:
--   Return True if string is a valid integer literal, possibly negative
--   but *not* just "-"
isNumericToken :: String -> Bool
isNumericToken "" = False
isNumericToken s =
  case s of
    ('-':rest) -> not (null rest) && all isDigit rest
    _          -> all isDigit s

-- formatOutput:
--   Takes the final (printedStr, stackStr) pair from interpretLine
formatOutput :: (String, String) -> String
formatOutput ("illegal","illegal") =
  -- EXACT format required by your assignment's samples:
  "(illegal,illegal)"

formatOutput (printed, stacked) =
  "(" ++ show printed ++ "," ++ show stacked ++ ")"
