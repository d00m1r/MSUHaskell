import System.IO  
import Control.Monad
import System.Environment
import Text.Printf
import Data.List
import Data.Function
import System.Cmd
import Data.Typeable

        --print . map readInt . words $ contents
-- alternately, main = print . map readInt . words =<< readFile "test.txt"


------------------------------------------------------------------
--ЗАДАЧА 8 --НЕ ПОЛУЧИЛОСЬ
--readMaybe «123» :: Int
-- main = do
--     xs <- getArgs
--     if null xs then do
--         putStrLn "You entered no arguments"
--      else do
--         print(xs)
        --print(2sums xs 0 "")

-- isInteger :: (Typeable a) => a -> Bool
-- isInteger n = typeOf n == typeOf 1

-- 2sums [] nums strings = (nums, strings)
-- 2sums (x:xs) nums strings
-- 	| False = 2sums xs (nums+x) strings
-- 	| otherwise = 2sums xs nums (strings++x)


------------------------------------------------------------------
--ЗАДАЧА 10 
--ЗАПУСК
--    ghc --make les5
--    ./les5

-- main = do
--     contents1 <- readFile "task10Input1"
--     contents2 <- readFile "task10Input2"
--     let arr1 = map readInt . words $ contents1
--     let arr2 = map readInt . words $ contents2
--     let newContents = allIntsToString (sort (arr1 ++ arr2))
--     when (length newContents > 0) $
--         writeFile "task10Output.txt" newContents

-- allIntsToString [] = ""
-- allIntsToString (x:xs) = show x ++ " " ++ allIntsToString xs 

-- readInt :: String -> Int
-- readInt = read




------------------------------------------------------------------
--ЗАДАЧА 12 -- НЕ ПОЛУЧИЛОСЬ
-- main = do
--   contents <- readFile "task12Input1"
--   print(contents)
--   print( lines contents)
--   --mapM_ f (lines fileContents)

-- f line = do
--   putStrLn (line)
--   --system $ "ghc -e " ++ show line






------------------------------------------------------------------
--ЗАДАЧА 13
-- ЗАПУСК ghc --make les5
--        ./les5 Hello
main = do
    xs <- getArgs
    contents <- readFile "task13Input"
    if null xs then do
        putStrLn "You entered no arguments"
     else do
     	print (manyCheck (stringArrToString xs) contents)



stringArrToString [] = ""
stringArrToString (x:xs) = x ++ stringArrToString xs
--выяснить есть ли подсписок в списке [1,3]    [1,4,1,3,8,9] на вход подаём подсписок
checkList :: Eq a => [a] -> [a] -> Bool
checkList y [] = False
checkList (y:ys) (x:xs) = 
    if y == x && checkPodlist ys xs then True 
    else checkList (y:ys) xs

checkPodlist :: Eq a => [a] -> [a] -> Bool
checkPodlist [] x = True
checkPodlist [y] (x:xs) = 
    if y == x && length [y] <= length (x:xs) then True else False
checkPodlist (y:ys) (x:xs) = 
    if y == x then checkPodlist ys xs else False

-------------------------------------------------
checkAndRemove :: Eq a => [a] -> [a] -> [a]
checkAndRemove y [] = []
checkAndRemove [] x = x
checkAndRemove (y:ys) (x:xs) = if checkPodlist (y:ys) (x:xs) == True
    then checkAndRemove ys xs
    else x:checkAndRemove (y:ys) xs

-- Удаляет все вхождения подсписка в список
manyCheck :: Eq a => [a] -> [a] -> [a]
manyCheck y [] = []
manyCheck y x = if checkList y x == True
    then manyCheck y (checkAndRemove y x)
    else x


