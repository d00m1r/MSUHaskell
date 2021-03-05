import Data.Char
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs)
    | length(x:xs) < 2 = (x:xs)
    | otherwise = quickSort(less) ++ [x] ++ quickSort(greater)
    where 
        less = filter (<=x) xs
        greater = [ i | i<-xs, i>x]

--------------------------------------
sum2lists [] [] = []
sum2lists x [] = error"incorrect pass"
sum2lists [] y = error"incorrect pass"
sum2lists (x:xs) (y:ys) = [ x+y ] ++ sum2lists xs ys

--------------------------------------
codeList :: [Char] -> [Int]
codeList [] = []
codeList (x:xs) = [ord x] ++ codeList xs

decodeList :: [Int] -> [Char]
decodeList [] = []
decodeList (x:xs) = [chr x] ++ decodeList xs

--------------------------------------
code :: Int -> String ->String
code p mess = map (\c -> chr $ ord c + p) mess

decode :: Int->String->String
decode p mess = code (negate p) mess

mySort :: (Ord a)=> [a]->[a]
mySort [] = []
mySort (x:xs) =                     
        let leftSort = mySort [a |a<-xs,a<=x]
            rightSort = mySort [a |a<-xs,a>x] 
        in leftSort++[x]++rightSort

quickSortWithoutGenerator :: Ord a => [a] -> [a]
quickSortWithoutGenerator [] = []
quickSortWithoutGenerator (x:xs)
    | length(x:xs) < 2 = (x:xs)
    | otherwise = quickSortWithoutGenerator(quickSortLess x xs) ++ [x] ++ quickSortWithoutGenerator(quickSortGreater x xs)

quickSortLess x [] = []
quickSortLess x (xs:xss)
    | x >= xs = [xs] ++ quickSortLess x xss
    | otherwise = quickSortLess x xss

quickSortGreater x [] = []
quickSortGreater x (xs:xss)
    | x > xs = quickSortGreater x xss
    | otherwise = [xs] ++ quickSortGreater x xss

-- sort2 :: Ord a => [a]->[a]
-- sort2 [] = []
-- sort2 [x] = [x]
-- sort2 (x1 : x2 : xs)
--    | (x1<x2) = let (a:as) = sort2 (x1:xs) 
--                 in a: sort2 (x2:as)   
--    | otherwise = let (a:as) = sort2 (x2:xs) 
--                 in a: sort2 (x1:as)
--------------------------------------
bubble1 :: (Ord a) => [a]->[a]
bubble1 (x:[]) = [x]
bubble1(x:y:xs)
    | x>y = y:bubble1 (x:xs)
    | otherwise = x:bubble1 (y:xs)

bubble :: (Ord a) => [a]->[a]
bubble [] = []
bubble lst = 
  let t1 =bubble1 lst
  in bubble (init t1)++[last t1]

someSort [] = []
someSort (x:[]) = [x]
someSort(xs) = someSort(removeNum i xs) ++ [i] 
    where i = maximum xs

removeNum x [] = []
removeNum x (xs:xss)
    | x==xs = xss
    | otherwise = xs:removeNum x xss

quickSortHalf :: Ord a => [a] -> [a]
quickSortHalf [] = []
quickSortHalf (xs)
    | length(xs) < 2 = xs
    | otherwise = quickSortHalf(less) ++ [xs!!half] ++ quickSortHalf(greater)
    where 
        half = div (length(xs)) 2
        listWithoutHalf = removeNum (xs!!half) xs
        less = [ i | i<-listWithoutHalf, i<=(xs!!half)]-- a = 1:1:1:[1..10]
        greater = [ i | i<-xs, i>(xs!!half)]

--------------------------------------
--Половина половин и bubble
removeElement :: Int-> [a] -> [a]
removeElement 1 (x:xs) = xs
removeElement k (x:xs) = x: removeElement (k-1) xs

quickSortHalfHalf [] = []
quickSortHalfHalf [x] = [x]
quickSortHalfHalf (xs) = quickSortHalfHalf(less1) ++ [halfhalf1] ++ quickSortHalfHalf(greater1) ++ [half] ++ quickSortHalfHalf(less2) ++ [halfhalf2] ++ quickSortHalfHalf(greater2)
    where 
        testhalf = div (length(xs)) 2 -- problems with minus
        testhalfhalf1 = div testhalf 2
        testhalfhalf2 = testhalf+testhalfhalf1
        --halfList = sortHalves testhalfhalf1 testhalfhalf2 testhalf [xs!!testhalfhalf1, xs!!testhalfhalf2, xs!!testhalf]
        halfList = someSort [xs!!testhalfhalf1, xs!!testhalfhalf2, xs!!testhalf]
        halfhalf1 = halfList!!0--min half
        halfhalf2 = halfList!!2--max half
        half = halfList!!1
        listWithoutHalf = removeNum (half) xs
        listWithoutHalfHalfLess = removeNum (halfhalf1) listWithoutHalf
        listWithoutHalfHalfGreater = removeNum (halfhalf2) listWithoutHalf
        less1 = [i | i<-listWithoutHalfHalfLess, i<=(halfhalf1)]-- a = 1:1:1:[1..10]
        greater1 = [i | i<-listWithoutHalfHalfLess, i>(halfhalf1) && i<=(half)]
        less2 = [i | i<-listWithoutHalfHalfGreater, i>(half) && i<=(halfhalf2)]
        greater2 = [i | i<-listWithoutHalfHalfGreater, i>(halfhalf2)]
--[5,1,2,5,6,7,0,-1,-100,100,1000,1]
--[-100,-1,0,1,1,2,5,5,6,7,100,1000]
-- sortHalves x y z xs = [xs!!minIndex] ++ [xs!!0] ++ [xs!!maxIndex]--ret el el el
--     where 
--         maxIndex = checkHalvesMax x y z xs
--         minIndex = checkHalvesMin x y z xs

-- remove2Num x y [] = []
-- remove2Num x y (xs:xss)
--     | x==xs || y==xs = remove2Num x y xss
--     | otherwise = xs:remove2Num x y xss

-- checkHalvesMax x y z xs
--     | max3 (xs!!x) (xs!!y) (xs!!z) == (xs!!x) = x
--     | max3 (xs!!x) (xs!!y) (xs!!z) == (xs!!y) = y
--     | max3 (xs!!x) (xs!!y) (xs!!z) == (xs!!z) = z
-- checkHalvesMin x y z xs
--     | min3 (xs!!x) (xs!!y) (xs!!z) == (xs!!x) = x
--     | min3 (xs!!x) (xs!!y) (xs!!z) == (xs!!y) = y
--     | min3 (xs!!x) (xs!!y) (xs!!z) == (xs!!z) = z

-- max3 :: (Ord) a=> a-> a-> a-> a
-- max3 x y z
--     | x >= y && x >= z = x
--     | y >= x && y >= z = y
--     | otherwise = z

-- min3 :: Ord a => a -> a -> a -> a
-- min3 x y z
--     | x <= y && x <= z = x
--     | y <= x && y <= z = y
--     | otherwise = z
