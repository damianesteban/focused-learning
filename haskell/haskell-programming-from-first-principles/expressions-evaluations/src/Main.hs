
-- Topic: Evaluation

triple :: Int -> Int
triple x = x * x

-- triple 2
-- [triple x = x * 3; x:= 2]
-- 2 * 3
-- 6

-- We applied `triple` to the value 2, then reduced it to 6.
-- `triple 2` is in canonical / normal form when it reaches 6 because it cannot
-- be reduced further

-- Floating has a type constraint
piTimesDoubleValue :: Floating a => a -> a
piTimesDoubleValue x = pi * (x * x)

q = 10
z = 11

f z = q + z
-- f 7
-- 17

-- Topic: Modular Arithmetic

-- Let’s say we need to write a function that will determine what day of 
-- the week it was or will be a certain number of days before or after this one
-- For our purposes here, we will assign a number to each day of the week, 
-- using 0 to represent Sunday.7 Then if today is Monday, and we want to know 
-- what day of the week it will be 23 days from now, we could do this:


determineDayOfWeek = mod (1 + 23) 7

-- The 1 represents Monday, the current day, while 23 is the number of days 
-- we’re trying to add. Using mod to wrap it around the 7 means it will 
-- return a number that corresponds to a day of the week in our numbering”

-- If we want to subtract and find out what day of the week it was 
-- some number of days ago `rem` won't work. 
-- Let’s try asking, if today is Wednesday (3), what day it was 12 days ago:

-- mod (3 - 12) 7
-- 5

-- rem (3 - 12) 7
-- -2

-- Topic: Parenthesization and $
-- The definition of thf `$` operator:
-- f $ a = f a

-- Immediately this seems a bit pointless until we remember that it’s defined
-- as an infix operator with the lowest possible precedence. 
-- The ($) operator is a convenience for when you want to express 
-- something with fewer pairs of parentheses:

-- Example:

randomFunction :: Int -> Int
randomFunction x = (x^) ( 2 + 2)

randomFunctionWithLessParens x = (x^) $ 2 + 2

-- The ($) will allow everything to the right of it to be evaluated first 
-- and can be used to delay function application.
-- NOTE: - This sounds like it could be a lot of fun :)

-- Topic: `let` and `in`

-- Example 1:
add1 :: Int -> Int
add1 x = 
  let inc = 1
  in x + inc

add4WithLet :: Int -> Int -> Int -> Int -> Int
add4WithLet w x y z = 
  let a = w + x
      b = y + a
  in z + b

add4WithWhere :: Int -> Int -> Int -> Int -> Int
add4WithWhere w x y z = 
    z + b
  where
    a = w + x
    b = y + a

-- Example 2:
-- In the REPL, this works:
-- let x = 5; y = 6 in x * y
-- In this file it can be written as:

multi x y = 
    x * y
  where
    x = 5
    y = 6

-- Example 3: Re-write the following with `where` clauses:
-- let x = 3; y = 1000 in x * 3 + y
-- let y = 10; x = 10 * 5 + y in x * 5
-- let x = 7
--    y = negate x
--    z = y * 10
--  in z / x + y”

addAndMultiplyTwoInts :: Int -> Int -> Int
addAndMultiplyTwoInts x y = 
    x * 3 + y
  where
    x = 3
    y = 1000

addAndMultiplyTwoIntsAgain :: Int -> Int -> Int
addAndMultiplyTwoIntsAgain x y =
    x * 5
  where
    y = 10
    x = 10 * 5 + y

-- Another exampel of a function that has a type constraint
addAndDivideThreeInts :: Fractional a => a -> a -> a -> a
addAndDivideThreeInts x y z =
    z / x + 7
  where
    x = 7
    y = negate x
    z = y * 10

-- TODO: - Practice let...in vs. where

-- More basic functions
waxOn :: Int -> Int
waxOn x =
    x * 5
  where
    x = 6

tripleWax :: Int -> Int
tripleWax x = x * 3

waxOff :: Int -> Int
waxOff x = triple x
