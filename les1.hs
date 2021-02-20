module Test where

-- ghci :l test.hs
-- :i /чекаем заголовок 
-- :r /reload
-- :m /выход из модуля в prelude

-- 3 + 3 ~ (+) 3 3 функциональная запись
-- fun 4 5 ~ 4 `fun` 5 операторная запись 
-- max 5 42 ~ (max 5) 42 частичное применение функции
-- sin 5 * 4 ~ sin (5) * 4 любой оператор имеет приоритет ниже, чем приоритет функции
-- f $ x = f x      sin (pi / 2) ~ sin $ pi / 2 правая ассоциативность
max1 :: (Ord) a=> a -> a -> a
max1 x y
    | x <= y =y
    | otherwise = x

max2 :: (Ord) a=> a -> a -> a -> a
max2 x y z
	| x <= y && y <= z || y<=x && x<=z =z
	| x <= z && z <= y || z<=x && x<=y =y
	| y<=z && z<=x || z<=y && y<=x =x

max3 :: (Ord) a=> a-> a-> a-> a
max3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

test3 :: Int -> String
test3 1 = "one"--образцы
test3 x = error "wrong"
test3 2 = "two"
test3 3 = "three"
test3 4 = "four"
test3 y = "no one no two no three"

--IF THEN ELSE
fun:: Int -> Int
fun x = if x > 0 
    then 1 
    else (-1)

--Задание своего оператора
-- ! # $ % * + . / < = > ? @ \ ^ | - ~ 
infixl 6 *-*
a *-* b = a*a - b*b

infixl 8 ≈ 
a ≈ b = a + b + 3

myhead1 :: [a] -> a
myhead1 [ ] = error "ssss"
myhead1 [x]=x
myhead1 (x:xs) = myhead1 [x]

main :: IO ()
main = return ()

-- поменять первый элемент на последний в списке
swapFirst :: [a] -> [a]
swapFirst [] = []
swapFirst x = last x : (tail x)


mylast :: [a] -> a
mylast [] = error "sos"
mylast [x] = x
mylast (_:xs) = mylast xs

--без встроенных функций
my1 :: [a]->[a]
my1 (x:xs) = xs ++[x]

mydelete2 :: [a] -> [a]
mydelete2 [] = error "dummy"
mydelete2 [x] = []
mydelete2 (x : xs) = x : mydelete2 (xs)

my2 :: [a]->[a]
my2 [] =[]
my2 (x:xs) =(mylast xs): my1 (x:mydelete2 xs)

-- поменять первый и последний
swap2 :: [a] -> [a]
swap2 [] = []
swap2 [a] = [a] 
swap2 x = last x : (init.tail $ x) ++ [head x]

swap1 :: [a] -> [a]
swap1 [] = []
-- swap1 x = if length x > 1
--     then last x : (init.tail $ x) ++ [head x]
--     else [x]
