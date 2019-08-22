module Main where

myGreeting :: String
myGreeting = "hello" ++ " friends!"

hello :: String
hello = "hello"

world :: String
world = "world"

-- Strings

-- Printing to the screen is an effectm so this 
-- must be wrapped in the IO type.

-- output: "hello world"
main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = concat [hello, " ", world]

-- Concatenation
-- ++ has the type [a] -> [a] -> [a]
-- concat has the type [[a]] -> [a]
-- concat flattens
-- Example:

makeItFlat :: [[a]] -> [a]
makeItFlat xs = concat xs

-- makeItFlat ["qq", "qww", "qq"]
-- "qqqwwqq"

-- makeItFlat [["qq"], ["qww"], ["qq"]]
-- ["qq","qww","qq"]

-- Concateation (prefix)

theGreeting :: String
theGreeting = (++) "hello" " you!"

bigGreeting :: IO()
bigGreeting = do
  putStrLn theGreeting
  putStrLn anotherGreeting
  where anotherGreeting =
          (++) hello ((++) " " world)

-- More list functions
-- A string is a specialized kind of list, so check it:

-- the (:) operator is cons, which builds a list:
funWithCons = 'c' : "hris"
-- "chris"

-- head returns the head of a list
funWithHead = head "Darth Vader"
-- 'D'

-- tail returns the tail of a list
funWithTail = tail "Darth Vader"
-- "arth Vader"

-- take returns the specified number of elements from the list:
funWithTake = take 1 "Luke"
-- "L"

-- drop returns the remainder of the list after the specified number of elements has been dropped
funWithDrop = drop 4 "Obi-Wan"
-- "Wan"

-- The infix operator (!!) returns the element that is in the specified position in the list. 
-- Note that this is an indexing function, and indices start from 0. 
-- That means the first element of your list is 0, not 1, when using this function:
funWithTakeAtIndex = "Darth Vader" !! 3
-- 't'

-- Building Functions

-- Given the list-manipulation functions mentioned in this chapter, 
-- write functions that take the following inputs and return the expected outputs. 

-- Given
-- "Curry is awesome"

-- Return
-- "Curry is awesome!"

addToString :: String
addToString = "Curry is awesome" ++ "!"

-- Given
-- "Curry is awesome"

-- Return
-- "y"

charFromString :: Char
charFromString = "Curry is awesome!" !! 4

-- Given
-- "Curry is awesome!"

-- Return
-- "awesome!"

dropNineFromString :: String
dropNineFromString = drop 9 "Curry is awesome!"

thirdLetter :: String -> Char
thirdLetter x = x !! 2

someLetter :: Int -> Char
someLetter x = "This is nice" !! x

-- NOTE: This is by NO means the best way to do this. But it works for now

-- Using the take and drop functions, see if you can write
-- a function called rvrs (an abbreviation of ‘reverse’ used because there 
-- is a function called ‘reverse’ already in Prelude, so if you call your 
-- function the same name, you’ll get an error message). rvrs should take 
-- the string “This is nice" and return the result “nice is This"

-- This may not be the most lovely Haskell code you will ever write, 
-- but it is quite possible using only what we’ve learned so far. 
-- First write it as a single function in a source file. 
-- This doesn’t need to, and shouldn’t, 
-- work for reversing the words of any sentence. 
-- You’re expected only to slice and dice this particular string 
-- with take and drop

rvrs phrase = 
  let x = drop 8 phrase
      y = take 4 phrase
  in x ++ " is " ++ y


-- Data Types

data Mood = Blah | Woot deriving Show

flipMood :: Mood -> Mood
flipMood mood =
  case mood of
    Woot -> Blah
    Blah -> Woot

-- Numeric Types

-- Integral Numbers
  -- Int
  -- Integer

-- Fractional
  -- Float
  -- Double
  -- Rational
  -- Scientific
  -- Example: type of the "/"" function :: Fractional a => a -> a -> a
-- Integral


