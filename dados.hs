--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c
--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b deriving (Eq,Show)


firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq,Show)

tuple1 (Tuple1 a) = Just a 
tuple1 (Tuple2 a _) = Just a 
tuple1 (Tuple3 a _ _) = Just a 
tuple1 (Tuple4 a _ _ _) = Just a 

tuple2 (Tuple2 _ b) = Just b 
tuple2 (Tuple3 _ b _) = Just b
tuple2 (Tuple4 _ b _ _) = Just b
tuple2 _ = Nothing

tuple3 (Tuple3 _ _ c) = Just c
tuple3 (Tuple4 _ _ c _) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 _ _ _ d) = Just d
tuple4 _ = Nothing 

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

getValue NIL = Nothing
getValue (Node a _ _) = Just a

getLeft NIL = Nothing
getLeft (Node _ left _) = Just left

getRight NIL = Nothing
getRught (Node _ _ right) = Just right

getParent _ NIL = NIL
getParent x (Node a left right) | (Just x == getValue left) || (Just x == getValue right) = (Node a left right)
                                | (x < a) = getParent x left
                                | otherwise = getParent x right
--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node a left right) | (getValue left /= Nothing) && (getValue left > Just a) = False
                          | (getValue right /= Nothing) && (Just a > getValue right) = False 
                          | otherwise = (isBST left) && (isBST right)
--insere uma nova chave na BST retornando a BST modificada

--Se estiver vazio coloca na primeira posição
--Se elemento maior que cabeça vai para direita
--Se elemento menor que cabeça vai para a esquerda
insert x NIL = Node x NIL NIL
insert x (Node a left right) | (x < a) = Node a (insert x left) right
                             | otherwise = Node a left (insert x right) 

--retorna o Node da BST contendo o dado procurado ou entao NIL
search x NIL = NIL
search x (Node a left right) | (x == a) = Node a left right
                             | (x < a) = search x left
                             | otherwise = search x right 

--retorna o elmento maximo da BST
mymaximum (Node a left right) | (right == NIL) = Just a
                            | otherwise = mymaximum left
--retorna o elemento minimo da BST
myminimum (Node a left right) | (left == NIL) = Just a
                              | otherwise = myminimum left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor = undefined

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento

--Cabeça , Esquerda , Direita
preOrder NIL = []
preOrder (Node x left right) = [x] ++ (preOrder left) ++ (preOrder right)

-- Esquerda , Cabeça , Direita
order NIL = []
order (Node x left right) = (order left) ++ [x] ++ (order right)

--Esquerda,Direita , Cabeça
postOrder NIL = []
postOrder (Node x left right) = (postOrder left) ++ (postOrder right) ++ [x]

