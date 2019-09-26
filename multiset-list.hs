module MultisetList ( 
                      module List
)
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import Data.List as List hiding (insert, union, sum)

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert elem [] = [(elem,1)]
insert elem bag = if dado == elem then [(dado, quant + 1)] ++ t else [h] ++ (MultisetList.insert elem t)
                where
                  h = head (bag)
                  t = tail (bag)
                  dado = fst h
                  quant = snd h
{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem bag = if (dado == elem && quant > 1) then [(dado, quant - 1)] ++ t else if (dado == elem && quant == 1) then t else [h] ++ (MultisetList.remove elem t)
                where
                  h = head (bag)
                  t = tail (bag)
                  dado = fst h
                  quant = snd h

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem [] = 0
search elem bag = if (dado == elem) then quant else (MultisetList.search elem t)
               where
                  h = head (bag)
                  t = tail (bag)
                  dado = fst h
                  quant = snd h

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union bag1 [] = bag1
union [] bag2 = bag2
union bag1 bag2 | (quant2 > (MultisetList.search dado2 bag1)) = MultisetList.union newA bag2
                   | otherwise = MultisetList.union bag1 t2 
              where
                h2 = head bag2
                t2 = tail bag2
                dado2 = fst h2
                quant2 = snd h2
                newA = MultisetList.insert dado2 bag1  

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
-- se quantA < quantB = add head bag2 se nao = add head bag1
intersection bag1 [] = []
intersection [] bag2 = []
intersection bag1 bag2 = if (quantA > quantB && quantB > 0) then [(dado1,quantB)] ++ intersection (tail bag1) bag2 
else if(quantB > 0) then [(dado1,quantA)] ++ intersection (tail bag1) bag2 else intersection (tail bag1) bag2
                 where
                    dado1 = fst(head bag1)
                    quantA = snd(head bag1)
                    quantB = search dado1 bag2 
                   
{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus bag1 [] = bag1
minus [] bag2 = []
minus bag1 bag2 = if(quant > 0) then [(dado,quant)] ++ minus (tail bag1) bag2 else minus (tail bag1) bag2
              where
                   qtde1 = snd(head bag1)
                   dado = fst(head bag1)
                   qdt2 = search dado bag2 
                   quant = qtde1 - qdt2
{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion [] [] = True
inclusion [] _ = True
inclusion _ [] = False
inclusion bag1 bag2 = if(qdt1 <= qdt2) then inclusion (tail bag1) bag2 else False
        where
             qdt1 = snd(head bag1)
             qdt2 = search (fst(head bag2)) bag2        



{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
mySum [] bag2 = bag2
mySum bag1 [] = bag1
mySum bag1 bag2 = mySum newA remA
          where
               dado1 = fst(head bag1)
               dado2 = fst(head bag2)
               remA = MultisetList.remove dado2 bag2
               newA = MultisetList.insert dado2 bag1

{-
 - Retorna a quantidade total de elementos no Bag
-}
size [] = 0
size bag = quant + MultisetList.size t
         where
             t = tail (bag)
             quant = snd (head bag)
