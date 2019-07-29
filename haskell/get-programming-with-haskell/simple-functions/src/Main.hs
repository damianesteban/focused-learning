
-- Basic math

inc :: Int -> Int
inc x = x + 1

double :: Int -> Int
double x = x * 2

square :: Int -> Int
square x = x^2

ifThen :: Int -> Int
ifThen n = if mod n 2 == 0
           then n - 2
           else 3 * n + 1

-- You and your friends are out getting pizza. On the menu are three sizes of pizza pie with three different prices:
-- 
-- 18 inches for $20
-- 16 inches for $15
-- 12 inches for $10
-- 
-- You want to know which choice gives you the most pizza for your dollar. 
-- You want to write a function that will give you the dollar-per-square-inch cost of the pizza”

calcCost :: Int -> Int -> String
calcCost inches dollars = "Your cost per square inch is: $ " ++ show ((fromIntegral inches) / (fromIntegral dollars)) 