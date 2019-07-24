module Main where

{-|
Functions that return Actions
  putStrLn is a function that prints 
  out a message to the console, right? 
  Not quite. Let’s look at its type. 
  
  What does it return?

  putStrLn :: String -> IO ()

  It’s a function that accepts a string, and returns an action
  to print something out to the console. 
  
  That’s an important distinction. 
  putStrLn doesn’t actually print anything, 
  it returns an instruction to print something. 
  Let’s call those instructions actions.

  Actions are essentially like VALUES, i.e. String or Bool
-}

main :: IO ()

-- Combining 3 actions
main = do
  putStrLn "Hello"
  putStrLn "Bob"
  putStrLn "How are you?"

doesNotWork :: Maybe Int
doesNotWork = do

  -- these work, because they are all 'Maybe a'
  Just 6
  Nothing

  -- error, this is of type IO. This won't work!
  -- putStrLn "Yo!"
  -- error, this is of type Int. This won't work!
  -- return 5

-- Examples
-- A do-block always yields whatever its last action does. 
-- If we put return last, we can make our do-block yield any simple value.
sayHello :: IO String
sayHello = do
  name <- getLine
  putStrLn ("Hello " ++ name)
  return name

beCareful :: Maybe Int
beCareful = do
  Just 6
  Nothing -- It will always return Nothing after Nothing - does that make sense? :)
  return 5