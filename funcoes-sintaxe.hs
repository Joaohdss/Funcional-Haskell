{-
- Dada uma tupla, divide o primeiro pelo segundo usando pattern matching.
- Ela deve ser indefinida quando o denominador for zero.
-}

divTuple :: (Float,Float) -> Float
divTuple (a, 0) = undefined
divTuple (a,b) = a/b
{-
 - Calcula o somatorio entre dois numeros a e b (a < b). Procure usar alguma funcao pronta sobre listas. 
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorio :: Int -> Int -> Int
somatorio a b | a > b = undefined
              | a <= b = sum [a..b]

{-
 - Calcula o somatorio (recursivo) entre dois numeros a e b (a < b).
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorioRec :: Int -> Int -> Int
somatorioRec a b | a > b = undefined
                 |otherwise = if (a+1 == b) then a+b else  a + somatorioRec (a+1) b

-- Defina a funcao que eleva um membro ao quadrado
square :: Int -> Int
square a = a*a

-- Soma os quadrados de dois numeros.
sumSquares :: Int -> Int -> Int
sumSquares a b = square a + square b

-- Defina uma funcao de alta ordem que aceita uma função (Int -> Int) e aplica a funcao a dois numeros
higherOrderSum f a b = sum (map f [a,b])

-- Defina a soma dos auqdrados em termos de higherOrderSum
hoSumSquares = undefined

--Implemente a funcao mapFilter que primeiro aplica o map de uma funcao f a uma lista e depois aplica a funcao filter
-- a lista resultante. Procure usar a composicao de funcoes
mapFilter f p xs = undefined
