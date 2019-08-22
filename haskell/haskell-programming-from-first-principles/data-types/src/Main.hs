module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Data Types: Bool
-- data Bool = False | True

-- Conditionals with if-then-else
-- Haskell doesn't have "if" statements,
-- it has "if expressions". This syntax
-- works with the Bool data type

-- Basic Examples

-- NOTE: Bool isn't in the function type, because we're talking about
-- inputs and outputs.
ifFalse :: [Char] 
ifFalse = if False then "t" else "f"

awesomeOrNotAwesome :: Int -> String -> String -> [Char]
awesomeOrNotAwesome x a w = if (x + 1 == 1) then a else w

-- awesomeOrNotAwesome 0 "AWESOME" "NOT AWESOME"
-- "AWESOME"

-- How does this function reduce?

-- Given: x = 0
-- if (x + 1 == 1) then "AWESOME" else "NOT AWESOME"
-- x is zero
-- if (1 == 1) then "AWESOME" else "NOT AWESOME"
-- evaluates to True
-- if True then "AWESOME" else "NOT AWESOME"
-- Goes the "path" of True
-- "AWESOME"