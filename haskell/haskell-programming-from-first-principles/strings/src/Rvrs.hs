-- Create a module for rvrs
module Rvrs where

rvrs :: String -> String
rvrs phrase = 
  let x = drop 8 phrase
      y = take 4 phrase
  in x ++ " is " ++ y

main :: IO ()
main = print $ rvrs "This is nice" 
-- NOTE: - By using the $ operator we avoid having to use parentheses