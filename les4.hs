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

--------------------------------------------------
elemInList :: Eq a =>a->[a]->Bool
elemInList a [] = False
elemInList a (x:xs)
    |a == x = True
    | otherwise = elemInList a xs

--------------------------------------------------
union1 :: Eq a => [a] -> [a] -> [a]
union1 [] [] = []
union1 x [] = x
union1 [] y = y
union1 (x:xs) (y:ys) = if x == y then x:(union1 xs ys) else x:y:(union1 xs ys)

deleleteClones :: Eq a => [a]->[a]
deleleteClones [] = []
deleleteClones (x:xs) 
    |elemInList x xs = deleleteClones xs
    |otherwise =  x: (deleleteClones xs)

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect _ [] = [] 
myIntersect (x:xs) y
    | elemInList x y = x : myIntersect xs y
    | otherwise = myIntersect xs y

myUnion :: Eq a => [a]-> [a]->[a]
myUnion [] [] = []
myUnion [] y = y
myUnion x [] = x
myUnion x y = deleleteClones (x++y)

--------------------------------------------------
symDifference :: Eq a => [a] -> [a] -> [a]
symDifference [] [] = []
symDifference [] y = y
symDifference x [] = x
symDifference x y = removeElements (myIntersect x y) (myUnion x y)

removeElem x [] = []
removeElem x (xs:xss)
    | x==xs = removeElem x xss
    | otherwise = xs:removeElem x xss
    
removeElements :: Eq a => [a] -> [a] -> [a]
removeElements [] [] = []
removeElements x [] = []
removeElements [] y = y
removeElements (x:xs) y = removeElements xs (removeElem x y)

