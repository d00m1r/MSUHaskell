import System.Environment
import Text.Printf
import Control.Exception
import System.CPUTime
import Data.Function (on)
import Data.List (sortBy)

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

-- unionBy (\x y -> x == y) [1,2,3,4] [1,2,3,4,5] => [1,2,3,4,5]

sumForce :: [Integer] -> Integer 
sumForce xs = sumForce' xs 0
         where sumForce' [] z = z 
               sumForce' (y:ys) z = 
                let s = z + y 
                in seq s  sumForce' ys s
               --seq отказ от ленивости


--------------------------------------------------
--Классический вариант на 2 части
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (xs)
    | length(xs) < 2 = xs
    | otherwise = quickSort(filter (< mid) xs) ++ filter (==mid) xs ++ quickSort(filter (> mid) xs)
    where 
        mid = xs !! (div (length(xs)) 2)
--------------------------------------------------
--Делим на 3 части
tripleQuickSort :: Ord a => [a] -> [a]
tripleQuickSort [] = []
tripleQuickSort [x] = [x]
tripleQuickSort x = tripleQuickSort (filter (<s) x)++ (filter (== s) x) ++ tripleQuickSort (filter (\k -> k>s && k<l) x) ++  (if l /= s then filter (== l) x else []) ++ tripleQuickSort (filter (>l) x)
    where
        low_mid xs = xs !! ((length xs) `div` 3)
        hi_mid xs = xs !! (((length xs) * 2) `div` 3)
        [s, l] =  sort2 [low_mid x, hi_mid x]

sort2 [x, y]
    | x <= y = [x, y]
    | otherwise = [y, x]
--------------------------------------------------
--Делим на 4 части
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort [x, y] = [min x y, max x y]
qsort x = qsort (filter (< s) x) ++ (filter (== s) x)
    ++ qsort (filter ((> s) .&&. (< m)) x) ++ (if m /= s then filter (== m) x else [])
    ++ qsort (filter ((> m) .&&. (< l)) x) ++ (if l /= m then filter (== l) x else [])
    ++ qsort (filter (> l) x) where
        mid xs = xs !! ((length xs) `div` 2)
        low_mid xs = xs !! ((length xs) `div` 4)
        hi_mid xs = xs !! (((length xs) * 3) `div` 4)
        [s, m, l] = sort3 [low_mid x, mid x, hi_mid x]

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

sort3 [x, y, z]
    | x >= y && y >= z = [z, y, x]
    | x >= z && z >= y = [y, z, x]
    | y >= x && x >= z = [z, x, y]
    | y >= z && z >= x = [x, z, y]
    | z >= x && x >= y = [y, x, z]
    | z >= y && y >= x = [x, y, z]
sort3 _ = error "Use sort3 only to sort lists of 3 elements"
--------------------------------------------------
--Проверка с выводом в файл

--import System.Environment
--ghc -o program-name ./name.hs
--time program-name 1 > sorted.txt
-- main = do
--     args <- getArgs
--     let x = (read (head args) :: Integer) * 20000000
--     print(tripleQuickSort[x, (x - 1)..1])

--start' <- start
--------------------------------------------------
-- import Text.Printf
-- import Control.Exception
-- import System.CPUTime
-- time :: IO t -> IO t
-- time a = do
--     start <- getCPUTime
--     v <- a
--     end   <- getCPUTime
--     let diff = (fromIntegral (end - start)) / (10^12)
--     printf "Computation time: %0.3f sec\n\n" (diff :: Double)
--     return v

-- main = do
--     putStrLn "sort func in Data.List"
--     time $ sort [10000000,9999999..0] `seq` return ()
--     putStrLn "quadro"
--     time $ qsort [10000000,9999999..0] `seq` return ()
--     putStrLn "triple"
--     time $ tripleQuickSort [10000000,9999999..0] `seq` return ()
--     putStrLn "classic quickSort"
--     time $ quickSort [10000000,9999999..0] `seq` return ()

-- -- Все перестановки элементов в списке
-- picks :: [t] -> [([t], t)]
-- picks [] = []
-- picks (x:xs) = [(xs,x)] ++ [(x:ys,y) | (ys,y) <- picks xs]  

-- perms :: [t] -> [[t]]
-- perms [] = [[]]
-- perms xs =          
--   do                     
--     (ys,x) <- picks xs        
--     zs     <- perms ys       
--     return (x:zs)             

permutations :: [a] -> [[a]]
permutations xs = [ y : zs | (y,ys)<-selections xs, zs<- permutations ys]


checkClones :: (Eq a) => [a] -> [a]
checkClones [] = []
checkClones (x:xs) = x : checkClones (filter (/= x) xs)

myPermutations x = checkClones (permutations x)

--------------------------------------------------
perest :: [a] -> [[a]]
perest [] = []   
perest [a] = [[a]]
perest (x:xs) = 
    let 
        blend a xs = (xs ++ [a]) : (iter' xs 0 [] (\x -> span' xs a x))

        span' xs a i = (fst (sp xs i)) ++ (a:(snd (sp xs i)))
        sp xs x = (take x xs, drop x xs) 
        iter' [] i acc f = acc
        iter' (x:xs) i acc f = iter' xs (i + 1) ((f i) : acc) f   
    in
          cp (\y -> blend x y) (perest xs)
    where
          cp f =  foldr ((++) . f) []

--------------------------------------------------
maxElemList :: (Foldable t, Ord a) => [t a] -> [a]
maxElemList [] = []
maxElemList (x:xs) = [maximum x] ++ maxElemList xs

maxElemList1 :: (Foldable t, Ord a) => [t a] -> [a]
maxElemList1 [] = []
maxElemList1 x = [ maximum i |i<-x ]

--------------------------------------------------
selections :: [a] -> [(a, [a])]
selections []     = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

findMaxDist :: (Ord t, Floating t) => [(t, t)] -> (((t, t), (t, t)), t)
findMaxDist x = maxDist [ findMax y ys 0.0 (0,0) | (y,ys)<-selections x ] 0 ((0,0),(0,0))

maxDist :: (Ord t, Floating t) => [((t, t), (t, t))] -> t -> ((t, t), (t, t)) -> (((t, t), (t, t)),t)
maxDist [] max points = (points, max)
maxDist (x:xs) max points
    |max >= newMax = maxDist xs max points
    |otherwise = maxDist xs newMax x
        where
            newMax = distance (fst(x)) (snd(x))

--(1,1)(2,2)(3,3)
findMax :: (Ord t, Floating t) => (t, t) -> [(t, t)] -> t -> (t, t) -> ((t, t), (t, t))
findMax x [] max second = (x, second) --[(first,second)]
findMax x (y:ys) max second
    |max >= newDist = findMax x ys max second
    |otherwise = findMax x ys newDist y
        where 
            newDist = distance x y 

distance :: Floating a => (a, a) -> (a, a) -> a
distance x y =  sqrt((fst(y) - fst(x))**2 + (snd(y) - snd(x))**2)

--Дано множество точек, найти треугольник минимального максимального периметра
--учесть если точки на одной линии, дабы не считать

length1 (x1, y1) (x2, y2) = (sqrt ((x1-x2)^2 + (y1-y2)^2))

maxLen [(x1,y1)] = 0
maxLen[(x1,y1),(x2,y2)] = length1 (x1,y1) (x2, y2)
maxLen ((x1,y1):(x2,y2):xs)
    |(length1 (x1,y1) (x2,y2)) >= maxLen((x1,y1):xs) = length1 (x1,y1) (x2, y2)
    |otherwise = maxLen((x1,y1):xs)

maxOf [(x1,y1)] = (0, [])
maxOf [(x1,y1),(x2,y2)] = (length1 (x1,y1) (x2, y2), [(x1,y1),(x2,y2)]) 
maxOf ((x1,y1):(x2,y2):xs) 
    | maxLen ((x1,y1):(x2,y2):xs) >= maxLen ((x2,y2):xs) && (maxLen ((x1,y1):(x2,y2):xs) /= (length1 (x1, y1) (x2, y2))) = maxOf ((x1,y1):xs) 
    | maxLen ((x1,y1):(x2,y2):xs) >= maxLen ((x2,y2):xs) && (maxLen ((x1,y1):(x2,y2):xs) == (length1 (x1, y1) (x2, y2))) = maxOf (((x1,y1):xs) ++ [(x2,y2)])
    | otherwise = maxOf ((x2,y2):xs)


------------------------------------------


getDist (x1,y1) (x2,y2) =
    return ((x2 - x1)*(x2 - x1) + (y2-y1) * (y2-y1), (x1,y1) , (x2,y2))

getMaxDist :: (Monad m, Ord b, Monoid a, Num b) => [(b, b)] -> m [(a, (b, (b, b), (b, b)))]
getMaxDist xs =
    return (sortBy (compare `on` snd) [getDist p q | p<-xs, q<-xs])
    
getMaxPoints xs = 
    return (last(last (getMaxDist xs)))
