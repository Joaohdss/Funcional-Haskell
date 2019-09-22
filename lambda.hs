--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
pow = \x -> \y -> if x == 0 then 0 else if y == 0 then 1 else x*(pow x (y-1))

fatorial = \x -> if (x == 0) then 1 else x * fatorial(x-1)

isPrime = \x -> x >= 2 && all (\n -> x `mod` n /= 0) [2..x-1]

fib = \x -> if (x == 0) then 0 else if (x == 1) then 1 else fib(x-1) + fib(x-2)

mdc = \x -> \y -> if (x `mod` y)  == 0 then y else if (y `mod` x) == 0 then x else if (x > y) then mdc y (mod x y) else  mdc x (mod y x)


mmc = \x -> \y -> head [z | z <-[(min x y)..x*y], z `mod` x == 0 , z `mod` y == 0]

coprimo = \x -> \y -> ((x `mdc` y) == 1)
goldbach x = undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

meuLast xs = case xs of [] -> error "Lista vazia!"
                        (h:t) -> (\h t -> if t == [] then h else meuLast t) h t

penultimo xs = (\n -> meuLast n)(init xs)

elementAt 1 xs = head xs
elementAt i xs = (\n -> \num -> elementAt (num-1) (tail n)) xs i

meuLength [] = 0
meuLength xs = (\n -> 1 + meuLength n) (tail xs)

meuReverso [] = []
meuReverso xs = (\n -> meuReverso (tail n) ++ [head n]) xs

isPalindrome xs = (\n -> n == meuReverso n) xs

compress [] = []
compress xs = (\n -> (compress (init xs))++[y | y <- [last n], not (y `elem` (init n))]) xs

compract [] = []
compact xs = (\n -> (head (filter (== (head n)) n)) : compact((removeList (head n) (tail n)))) xs

removeList e [] = []
removeList e (x:xs)
 | e == x = removeList e xs
 | otherwise = x:(removeList e xs) 

existe a [] = 0
existe a xs = (\a -> \xs -> (if a == (head xs) then 1+existe a (tail xs) else existe a (tail xs)))a xs

encode [] = []
encode xs = (\xs ->(head xs , existe (head xs) (tail xs)) : encode (removeList (head xs) (tail xs))) xs

split = \xs -> \i -> [take i xs] ++ [drop i xs]

slice = \xs -> \imin -> \imax -> take (imax-1) (drop (imin-1) xs) 

insertAt el pos xs = undefined

sort xs = undefined

mySum = \xs -> foldr (+) 0 xs

maxList = \xs -> if xs == [] then 0 else foldr (max) 0 xs


buildPalindrome = \xs -> if (meuLength xs == 0) then [] else [(head xs)] ++ buildPalindrome (tail xs) ++ [(head xs)]

mean = \xs -> mySum xs `div` meuLength xs

myAppend::([lista]->[lista]->[lista])
myAppend = \xs-> \ys -> foldr (:) ys xs

