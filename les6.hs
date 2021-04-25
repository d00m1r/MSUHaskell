-- есть наборы координат, и радиусов окружности x_i y_i r_i есть ли такие кругие, которые не пересекаются с остальными
-- findRadius [] [] = []
-- findRadius (x,y) r = []

-- --находится внутри
-- isIn point1 point2 r1 r2
--пересечение радиусов внешне
--checkOneForAll :: Floating a => (a,a) -> a -> [(a, a)] -> [a] -> [(a,a)] -> [(a,a)]
--checkAll :: (Ord t, Floating t) => [(t, t)] -> [t] -> [[(t, t), (t, t)]] -> [[(t, t), (t, t)]]
checkAll [] [] res = res
checkAll (p:points) (r:radius) res 
    |null checkOnePoint = checkAll points radius res
    |otherwise = checkAll points radius (res ++ checkOnePoint)
        where 
            checkOnePoint = checkOneForAll p r points radius [] 

-- checkOneForAll :: (Ord t, Floating t) => (t, t) -> t -> [(t, t)] -> [t] -> [[(t, t), (t, t)]] -> [[(t, t), (t, t)]]
checkOneForAll p r [] [] res = res
checkOneForAll  p r (ps:points) (rs:radius) res
    | isIntersect p ps r rs == True = checkOneForAll p r points radius res
    | otherwise = checkOneForAll p r points radius (res ++ [[p] ++ [ps]])

isIntersect :: (Ord a, Floating a) => (a, a) -> (a, a) -> a -> a -> Bool
isIntersect point1 point2 r1 r2
    | (distance point1 point2) <= r1 + r2 = True
    | otherwise = False

distance :: Floating a => (a, a) -> (a, a) -> a
distance x y =  sqrt((fst(y) - fst(x))**2 + (snd(y) - snd(x))**2)