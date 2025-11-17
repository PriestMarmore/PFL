{-
  L.EIC: Functional and Logic Programming Project
  Extended Calculator with Subtraction, Division, Modulus, Variables, and Commands.
-}
module Main where

import Parsing
import Data.Char

-- NOTE: Removed 'Distribution.Simple.Command' import as it's not needed
-- and was causing a dependency error.

-- =====================================
-- Part 2: State Definitions (Environment and Commands)
-- =====================================

type Name = String
type Env = [(Name, Integer)] -- Environment maps variable names to their Integer values

data Command = Assign Name Expr  -- Command for assignment (e.g., x=10)
             | Eval Expr         -- Command for simple expression evaluation (e.g., x+5)
             deriving Show

-- =====================================
-- Part 1 & 2: Expression Data Type
-- =====================================

data Expr = Num Integer
          | Var Name            -- New: Variable lookup
          | Add Expr Expr
          | Sub Expr Expr       -- Part 1: Subtraction
          | Mul Expr Expr
          | Div Expr Expr       -- Part 1: Integer Division
          | Mod Expr Expr       -- Part 1: Modulus/Remainder
          deriving Show

-- =====================================
-- Part 2: Environment Helpers
-- =====================================

-- Helper function to find a variable's value in the environment
-- Crashes on undefined variable, as permitted by instructions.
lookupEnv :: Name -> Env -> Integer
lookupEnv name env = case lookup name env of
                       Just val -> val
                       Nothing  -> error ("Undefined variable: " ++ name)

-- Helper function to update the environment with a new binding.
-- New binding goes to the head of the list, effectively shadowing old ones.
updateEnv :: Name -> Integer -> Env -> Env
updateEnv name val env = (name, val) : filter ((/= name) . fst) env

-- =====================================
-- Part 1 & 2: Evaluation
-- =====================================

-- New internal evaluation function (Expr -> Integer) using Env
evalEnv :: Env -> Expr -> Integer
evalEnv env (Num n)     = n
evalEnv env (Var name)  = lookupEnv name env -- Part 2: Variable lookup
evalEnv env (Add e1 e2) = evalEnv env e1 + evalEnv env e2
evalEnv env (Sub e1 e2) = evalEnv env e1 - evalEnv env e2  -- Part 1
evalEnv env (Mul e1 e2) = evalEnv env e1 * evalEnv env e2
evalEnv env (Div e1 e2) = evalEnv env e1 `div` evalEnv env e2 -- Part 1 (integer division)
evalEnv env (Mod e1 e2) = evalEnv env e1 `mod` evalEnv env e2 -- Part 1

-- =====================================
-- Part 1 & 2: Parsers
-- =====================================

-- Parser for variables (sequence of one or more letters)
variableP :: Parser Name
variableP = many1 (satisfy isLetter)

expr :: Parser Expr
expr = do t <- term
          exprCont t

-- Part 1: Extended to handle '+' and '-'
exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> do char '-'
                      t <- term
                      exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

-- Part 1: Extended to handle '*', '/' and '%'
termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                <|> do char '/'
                       f <- factor  
                       termCont (Div acc f)
                <|> do char '%'
                       f <- factor  
                       termCont (Mod acc f)
                <|> return acc

-- Part 2: Updated to parse variable, then natural, then parentheses.
-- NOTE: Corrected your previous code which had 'natural' listed twice.
factor :: Parser Expr
factor = do v <- variableP          -- New: Try parsing a variable first
            return (Var v)
         <|> do n <- natural        -- Then parse a number
                return (Num n)
         <|> do char '('            -- Then parse a parenthesized expression
                e <- expr
                char ')'
                return e
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)


-- Part 2: Top-level parser for commands (Assignment or Expression)
commandP :: Parser Command
commandP = assignmentP <|> expressionP -- Simplified structure

  where
    -- Assignment: variable '=' expression
    -- If this fails *before* or *at* the '=', the expressionP gets a clean shot.
    assignmentP :: Parser Command
    assignmentP = do v <- variableP
                     char '='   -- Must see the '=' immediately after the variable
                     e <- expr
                     return (Assign v e)

    -- Expression: just an expression
    -- This handles everything else, including simple variable lookup (e.g., 'x')
    expressionP :: Parser Command
    expressionP = do e <- expr
                     return (Eval e)

----------------------------------------------------------------             
  
-- Part 2: Top-level Execution and IO Loop

main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt) -- Initialize calculator with empty environment []

-- | Read-eval-print loop
-- Carries the environment (Env) between commands
calculator :: Env -> [String] -> IO ()
calculator _ []  = return ()
calculator env (l:ls) = do
    let (output, newEnv) = execute env l -- Execute command, get result and new Env
    putStrLn output
    calculator newEnv ls -- Pass the new environment to the next command

-- | Executes a single command string using the current environment
execute :: Env -> String -> (String, Env)
execute env input =
  case parse commandP input of
    [(cmd, "")] -> processCommand env cmd
    -- Handles parse failures by printing an error and returning the original Env
    _           -> ("parse error; try again", env)
  where
    -- Processes a parsed Command
    processCommand :: Env -> Command -> (String, Env)
    
    -- Assignment: Evaluate expression, update environment, and output value
    processCommand env (Assign name expr) =
      let val = evalEnv env expr
          newEnv = updateEnv name val env
          output = show val
      in (output, newEnv)

    -- Evaluation: Evaluate expression and output value; environment remains unchanged
    processCommand env (Eval expr) =
      let val = evalEnv env expr
          output = show val
      in (output, env)