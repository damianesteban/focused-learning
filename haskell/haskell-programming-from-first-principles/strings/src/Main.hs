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

-- TODO: Building Functions p. 110



