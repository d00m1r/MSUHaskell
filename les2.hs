import Data.Ratio
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
--map (>0)[x|x<-[1..10]] возвращает список [True,True,...]
    |checkPodlist (map (\x-> x>0 || x<=0) y) ([ f a | a<-y ]) = True
    | otherwise = False

-------------------------------------------------
 -- написать функцию которая перемножает матрицы

myLength :: [a] -> Int
myLength [] = 0
myLength [a] = 1
myLength (x:xs) = 1 + myLength xs

-- for i in range(len(A)):
--         for j in range(len(B[0])):
--             for k in range(len(B)):
--                 C[i][j] += A[i][k]*B[k][j]
-- Python

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

getrow :: (Num t, Eq t) => [[a]] -> t -> [a]
getrow [] y = []
getrow (x:xs) y = if y == 1 then x else getrow xs (y-1)

getcol :: [[a]] -> Int -> [a]
getcol [] y = []
getcol (x:xs) y = [x !! (y-1)] ++ getcol xs y

mult_vector :: Num a => [a] -> [a] -> a
mult_vector xs ys = sum[(xs!!j) * (ys!!j) | j<-[0..length xs -1]]

mult :: (Eq p, Num p, Num a) => [[a]] -> [[a]] -> p -> Int -> a
mult x y i j = mult_vector row col
    where
        row = getrow x i
        col = getcol y j

mult_matrix :: Num a => [[a]] -> [[a]] -> [[a]]
mult_matrix x y = [[mult x y i j|j<-[1..len_y]]|i<-[1..len_x]]
    where
        len_x = length(x)
        len_y = length(head y)

matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]] 
matrixMultiply a b = map (mult1 [] b) a 
    where 
        mult1 xs [] _ = xs 
        mult1 xs _ [] = xs 
        mult1 [] (x':xs') (y:ys) = mult1 (map (y *) x') xs' ys 
        mult1 xs (x':xs') (y:ys) = mult1 (zipWith (\u v -> u + v * y) xs x') xs' ys

mult1:: Num a => [[a]] -> [[a]] -> [[a]]
mult1 uss vss = map ((\xs -> if null xs then [] else
    foldl1 (zipWith (+)) xs). zipWith (\vs u -> map (u*) vs) vss) uss
-- null если список пуст то True иначе False

-------------------------------------------------
-- Свертка
foldrm:: (a->b->b)->b->[a]->b
foldrm f x0 [] = x0
foldrm f x0 (x:xs) = f x (foldrm f x0 xs)

foldlm:: (b->a->b)->b->[a]->b
foldlm f x0 [] = x0
foldlm f x0 (x:xs) = foldlm f (f x0 x) xs

summa :: (Num a)  => [a] ->a
summa xs = foldlm (\acc x -> acc + x) 0 xs

summa_right :: (Num a)  => [a] ->a
summa_right = foldrm (+) 0 

product_right :: (Num a)  => [a] ->a
product_right = foldrm (*) 1

summa1 :: (Num a)  => [a] ->a
summa1 xs = foldlm (\acc x -> acc - x) 0 xs

contactination :: [[a]]->[a]
contactination = foldrm (++) []

elem1 :: (Eq a)=>a->[a]->Bool
elem1 x = foldlm (\s x1 -> if x1==x then True else s) False

------------------------------------------------
-- Вычисление определителя

--удаление строки
dely1 :: Int-> [a] -> [a]
dely1 1 (x:xs) = xs
dely1 k (x:xs) = [x] ++ dely1 (k-1) xs

--удаление столбца
deletesty::(Num a) => Int -> [[a]] -> [[a]]
deletesty k x = [c |c<- [ dely1 k q | q<-x ]]

determinant :: Num a => [[a]] -> a
determinant [] = error "Empty Error"
determinant [[x]] = x
determinant (x:xs) = 
    sum [(-1)^(j+1)* (head [x])!!(j-1) * determinant(deletesty j xs) | j<-[1..(length x) ]]
--head$head[[1,2],[3,4]] -> 1
--head(head[[1,2],[3,4]]) -> 1

-----------------------------------------------
--Вычисление обратной матрицы
-- a = [[1,2,3],[4,1,6],[7,1,1]]
complementMatrix :: Num a => [[a]] -> [[a]]
complementMatrix x = [[(-1)^(i+j) * determinant(deletesty j (dely1 i x)) | j<-[1..len]] | i<-[1..len]]
    where
        len = length (x!!0)

inverseMatrix :: Integral a => [[a]] -> [[Ratio a]]
inverseMatrix x
    | determinant x == 0 = error "Inverse matrix does not exist"
    | otherwise = (map.map) (% determinant x) (transpose(complementMatrix x))
    -- (map.map) (%2) [[1,2],[3,4]]
    -- map           :: (a -> b) ->   [a]   ->   [b]
    -- (map.map)     :: (a -> b) ->  [[a]]  ->  [[b]]
    -- (map.map.map) :: (a -> b) -> [[[a]]] -> [[[b]]]

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose [[x]] = [[x]]
transpose x = [map head x] ++ transpose(map tail x) 

-----------------------------------------------
--Разбить его на подсписки
podspisok :: [Int] ->[[Int]]
podspisok []  = [[]]
podspisok (x:xs) = [frstn (x:xs) n | n <- [1..length (x:xs)]] ++ podspisok xs
    where frstn x n | length x < n = []
          frstn _ 0 = []
          frstn (x:xs) n = x : frstn xs (n-1)
 ---------------------------
podspisok11:: Eq a=> [a] -> [[a]]
podspisok11 [] = [[]]
podspisok11 (x:xs) = pods [x] xs ++ podspisok11 xs
    where
        pods a [] = [a]
        pods a (y:ys) = a:pods (a++[y]) ys

-- sublist (x:xs) = [ if i == x then x else sublist(xs)| i<-[1..length (x:xs)]] ++ sublist xs
--Упорядочить список и ДОДЕЛАТЬ
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs)
    | length(x:xs) < 2 = (x:xs)
    | otherwise = quickSort(less) ++ [x] ++ quickSort(greater)
    where 
        less = filter (<=x) xs
        greater = [ i | i<-xs, i>x]



