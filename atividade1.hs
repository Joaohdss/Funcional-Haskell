xor True False = True
xor False True = True
xor _ _ = False

impl a b = (not a) || b

equiv a b = (impl a b) && (impl b a)

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x y | (x == 0) = 0
        | (y == 0) = 1
        | otherwise = x*(pow x (y-1))

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial x | (x == 0) = 1
           | otherwise = x * fatorial(x-1)
{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
  
isPrime x = x >= 2 && all (\n -> mod x n /= 0) [2..x-1]      

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib x | (x == 0) = 0
      | (x == 1) = 1
      | x > 1 = fib(x-1) + fib(x-2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc a b | mod a b == 0 = b
        | mod b a == 0 = a
        | a > b = mdc b (mod a b)
        | a < b = mdc a (mod b a)

mdcEuclides 0 b = b
mdcEuclides a 0 = a
mdcEuclides a b = mdcEuclides b y
    where
        y = mod a b
{-
 Calcula um MMC de dois numeros
 divi retorna True se n é divisivel por a b
-}

mmc a b = head resul -- retorna a cabeça da lista
    where resul = filter (divi a b) [(min a b)..a*b] 
divi a b n = (mod n a == 0) && (mod n b == 0)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}

comprimo a b = (mdcEuclides a b) == 1

