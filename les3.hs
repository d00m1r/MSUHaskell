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
quickSortHalfHalf (xs) = 
    let less1 = [i | i<-listWithoutHalfHalfLess, i<=(halfhalf1)]
        greater1 = if halfhalf1 /= half then [i | i<-listWithoutHalfHalfLess, i>(halfhalf1) && i<=(half)] else [i | i<-listWithoutHalfHalfLess, i>(halfhalf1) && i<(half)]
        less2 = if halfhalf2 /= half then [i | i<-listWithoutHalfHalfGreater, i>(half) && i<=(halfhalf2)] else [i | i<-listWithoutHalfHalfGreater, i>(half) && i<(halfhalf2)]
        greater2 = [i | i<-listWithoutHalfHalfGreater, i>(halfhalf2)]
    in quickSortHalfHalf(less1) ++ [halfhalf1] ++ quickSortHalfHalf(greater1) ++ [half] ++ quickSortHalfHalf(less2) ++ [halfhalf2] ++ quickSortHalfHalf(greater2)
    where 
        testhalf = quot (length(xs)) 2 -- problems with minus
        testhalfhalf1 = quot testhalf 2
        testhalfhalf2 = testhalf+testhalfhalf1
        --halfList = sortHalves testhalfhalf1 testhalfhalf2 testhalf [xs!!testhalfhalf1, xs!!testhalfhalf2, xs!!testhalf]
        halfList = someSort [xs!!testhalfhalf1, xs!!testhalfhalf2, xs!!testhalf]
        halfhalf1 = halfList!!0--min half
        halfhalf2 = halfList!!2--max half
        half = halfList!!1
        listWithoutHalf = removeNum (half) xs
        listWithoutHalfHalfLess = removeNum (halfhalf1) listWithoutHalf
        listWithoutHalfHalfGreater = removeNum (halfhalf2) listWithoutHalf

mergesort'merge :: (Ord a) => [a] -> [a] -> [a]
mergesort'merge [] xs = xs
mergesort'merge xs [] = xs
mergesort'merge (x:xs) (y:ys)
    | (x < y) = x:mergesort'merge xs (y:ys)
    | otherwise = y:mergesort'merge (x:xs) ys
 
mergesort'splitinhalf :: [a] -> ([a], [a])
mergesort'splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = mergesort'splitinhalf xs
--[5,1,2,5,6,7,0,-1,-100,100,1000,1]
--[-100,-1,0,1,1,2,5,5,6,7,100,1000]

--------------------------------------
--Сортировка Подсчетом(работает и с отрицательными)
countingSort :: (Num a, Enum a, Ord a) => [a] -> [a]
countingSort [] = []
countingSort x = printByIndex xMin [ countElemInList x i |i<-[xMin..xMax]]
    where
        xMin = minimum x
        xMax = maximum x

printByIndex index [] = []
printByIndex index (x:xs) 
    | x < 1 = printByIndex (index+1) xs
    | otherwise = index:printByIndex index (newX:xs)
    where
        newX = x - 1

--Кол-во вхождений элемента в список
countElemInList :: Eq a => [a] -> a -> Int
countElemInList [] f = 0
countElemInList (x:xs) f 
    | f == x = 1 + (countElemInList xs f)
    | otherwise = countElemInList xs f

--------------------------------------
indexElw :: (Eq a) => [a] -> a -> Int -> Int
indexElw [] _ _ = (-1)
indexElw (x:xs) y n | (x==y) = n
                   | otherwise = indexElw xs y (n+1)
-- import Data.Array
-- countingSort :: (Ix n) => [n] -> n -> n -> [n]
-- countingSort l lo hi = concatMap (uncurry $ flip replicate) count
--   where count = assocs . accumArray (+) 0 (lo, hi) . map (\i -> (i, 1)) $ l
myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x:xs) = if x > myMaximum xs then x else myMaximum xs

--------------------------------------------------
listLength :: [a] -> Int
listLength = foldl (\n _ -> n + 1) 0

--разбивает на части [первые n элементов] [всё остальное]
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 xs = ([], xs)
mySplitAt n [] = error "Too short list"
mySplitAt n (x:xs) = (x:left, right)
                   where
                       (left, right) = mySplitAt (n - 1) xs

--при этом сортирует
mergeLists :: (Ord a) => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys) = if x <= y 
                                then x : (mergeLists xs (y:ys))
                                else y : (mergeLists (x:xs) ys)


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeLists (mergeSort leftPart) (mergeSort rightPart)
             where
                (leftPart, rightPart) = (take half xs, drop half xs) -- splitListToHalves
                half = div (length xs) 2
--------------------------------------------------



