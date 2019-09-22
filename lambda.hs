--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
pow = \x -> \y -> if x == 0 then 0 else if y == 0 then 1 else x*(pow x (y-1))

fatorial = \x -> if (x == 0) then 1 else x * fatorial(x-1)

isPrime = \x -> x >= 2 && all (\n -> x `mod` n /= 0) [2..x-1]

fib = \x -> if (x == 0) then 0 else if (x == 1) then 1 else fib(x-1) + fib(x-2)

mdc = \x -> \y -> if (x `mod` y)  == 0 then y else if (y `mod` x) == 0 then x else if (x > y) then mdc y (mod x y) else  mdc x (mod y x)


mmc x y = undefined
coprimo x y = undefined
goldbach x = undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast xs = undefined
penultimo xs = undefined
elementAt i xs = undefined
meuLength xs = undefined
meuReverso xs = undefined
isPalindrome = \xs -> if xs == [] then True else (head xs == tail xs)
compress xs = undefined
compact xs = undefined
encode xs = undefined
split xs i = undefined
slice xs imin imax = undefined
insertAt el pos xs = undefined
sort xs = undefined
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean xs = undefined
myAppend xs ys = undefined
