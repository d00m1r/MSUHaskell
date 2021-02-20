maxNum :: Ord a => [a] -> a
maxNum [a] = a
maxNum (x:y:xs) = maxNum (max1 x y : xs)

minNum :: Ord a => [a] -> a
minNum [a] = a
minNum (x:y:xs) = minNum (min1 x y : xs)

max1 :: (Ord) a=> a -> a -> a
max1 x y
    | x <= y =y
    | otherwise = x

min1 :: (Ord) a=> a -> a -> a
min1 x y
    | x >= y =y
    | otherwise = x
-- Поиск минимального и максимального значения в списке
-----------------------------------------
maxMin :: (Ord) a => [a] -> [a]
maxMin [] = error "empty"
maxMin [a] = [a]
maxMin [x,y] = if x<y then [x,y] else [y,x]
maxMin x = [minNum x, maxNum x]
-----------------------------------------
min_max :: (Ord a => [a] -> [a])
min_max [] = error "empty"
min_max [x] = [x]
min_max [x,y]
          | x == y = [x]
          | x > y = [y,x]
          | otherwise = [x,y]
min_max (x:y:z:xs)
            | x > y = min_max (y:x:xs)
            | z < x = min_max (z:y:xs)
            | z > y = min_max (x:z:xs)
            | otherwise = min_max (x:y:xs)

min_max1 :: (Ord a) => [a]->[a]
min_max1 [] = error "sos"
min_max1 [x] = [x,x]
min_max1 [x,y] = if x<y then [x,y] else [y,x]
min_max1 (x:y:xs)
    | [ k| k<-(y:xs), k>=x] /= [] && [ k| k<-(y:xs), k<=x] /= [] = min_max1 (y:xs)
    | otherwise = min_max1 ((y:xs)++[x])

-- Убрать элемент из списка ---------------------
removeElement :: Int-> [a] -> [a]
removeElement 1 (x:xs) = xs
removeElement k (x:xs) = x: removeElement (k-1) xs
-------------------------------------------------

--убрать дубли в списке [1,2,1,3,1] ->[1,2,3]
removeClones :: Eq a => [a] -> [a]
removeClones [] = []
removeClones [a] = [a]
removeClones (x:xs) = x : removeClones(removeNum x (xs))

removeNum :: Eq a => a -> [a] -> [a]
removeNum num [] = []
removeNum num (x:xs) = 
    if x == num then removeNum num xs 
    else x : removeNum num xs
-------------------------------------------------

--выяснить есть ли подсписок в списке [1,3]    [1,4,1,3,8,9] на вход подаём подсписок
checkList :: Eq a => [a] -> [a] -> Bool
checkList y [] = False
checkList (y:ys) (x:xs) = 
    if y == x && checkPodlist ys xs then True 
    else checkList (y:ys) xs

--Проверяет чтобы подсписок совпадал с началом всего списка 
-- [1,2] [1,2,3,4,5] True.  [2,3] [1,2,3,4,5] 
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
-------------------------------------------------
-- Есть ли дубликаты в списке?
checkClones :: Eq a => [a] -> Bool
checkClones [] = False
checkClones (x:xs)
    | (checkList [x] xs) == True = True
    | otherwise = checkClones xs

-------------------------------------------------
-- import Data.Ratio
-- 1%3 + 5%17 ~ 32%51

-- import Data.Char 
-- ord 'a'
-- chr '97'

-- map не меняет список
-- filter меняет список
-- a = map (*) [1..]
-- (a!!5)2
-- a = map ($) [sin,cos,sqrt,(+1)] список операций
-- (a!!2)2

mapmy :: (a -> b) -> [a] -> [b]
mapmy _ [] = []
mapmy f (x:xs) = f x:mapmy f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1  f (x:xs) = if (f x) then x:(filter1 f xs) else filter1 f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2  f (x:xs)
    | (f x) = x:(filter2 f xs) 
    | otherwise = filter2 f xs

filter3 :: (a -> Bool) -> [a] ->[a]
filter3 _ [] = []
filter3 f (x:xs) = [ x| f x] ++ filter3 f xs

-------------------------------------------------
mymap :: (a -> b) -> [a] -> [b]
mymap _[] = []
mymap f y =  [f a | a<-y]
 -- import Data.List
 -- any (>0) [1..10] если хотя бы одно удовл то вернёт True
-- any (\x-> x*5 == 40) [1,3,5,8] ~ True
myany :: (t -> Bool) -> [t] -> Bool
myany _ [] = False
myany f y 
    |checkList ([True]) ([ f a | a<-y ]) = True
    | otherwise = False

myall :: (Ord t, Num t) => (t -> Bool) -> [t] -> Bool
myall _ [] = False
myall f y 
--map (>0)[x|x<-[1..10]]
    |checkPodlist (map (\x-> x>0 || x<=0) y) ([ f a | a<-y ]) = True
    | otherwise = False

-------------------------------------------------
 -- написать функцию которая перемножает матрицы

myLength :: [a] -> Int
myLength [] = 0
myLength [a] = 1
myLength (x:xs) = 1 + myLength xs

-- multiplicationOfElements a b i j k
--     if i < (myLength a - 1) then
--         [if j < myLength (b!!0) - 1 
--             then [(multiplicationOfElementsK a b 0 i j)] ++ multiplicationOfElements a b i (j+1)
--             else [multiplicationOfElementsK a b 0 i j] 
--         ] ++ multiplicationOfElements a b (i+1) j k
--     else  [multiplicationOfElements a b i j k]
--?? Как объединить их все в одну функцию связать через множественные if else 
multiplicationOfElementsI :: Num a => [[a]] -> [[a]] -> Int -> [[a]]
multiplicationOfElementsI a b i
    | i < (myLength a - 1) = [(multiplicationOfElementsJ a b i 0)] ++ multiplicationOfElementsI a b (i+1)
    |otherwise = [multiplicationOfElementsJ a b i 0]

multiplicationOfElementsJ :: Num a => [[a]] -> [[a]] -> Int -> Int -> [a]
multiplicationOfElementsJ a b i j
    | j < myLength (b!!0) - 1 = [(multiplicationOfElementsK a b 0 i j)] ++ multiplicationOfElementsJ a b i (j+1)
    |otherwise = [multiplicationOfElementsK a b 0 i j]

multiplicationOfElementsK :: Num a => [[a]] -> [[a]] -> Int -> Int -> Int -> a
multiplicationOfElementsK a b k i j
    | k < (myLength b - 1) = (a!!i!!k) * (b!!k!!j) + multiplicationOfElementsK a b (k+1) i j
    | otherwise = (a!!i!!k) * (b!!k!!j)

matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication x y 
    | matrixCheck x y == False = error "uncorrect input"
    | otherwise = multiplicationOfElementsI x y 0

-- a = [[1,2],[3,4]]
-- b = [[2,3,4],[5,6,7]]
matrixCheck :: [[a1]] -> [[a2]] -> Bool
matrixCheck [] [] = False
matrixCheck x [] = False
matrixCheck [] y = False
matrixCheck (x:xs) (y:ys) 
    | myLength x == myLength (y:ys) && checkRows (myLength x) xs == True && checkRows (myLength y) ys == True = True
    | otherwise = False

checkRows :: Int -> [[a]] -> Bool
checkRows lengthX [] = True
checkRows lengthX [x] 
    | lengthX /= myLength x = False
    | otherwise = True
checkRows lengthX (xs:xss) 
    | lengthX /= (myLength xs) = False
    | otherwise = checkRows lengthX xss
    
-------------------------------------------------





