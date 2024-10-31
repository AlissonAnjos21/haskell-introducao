--1. Utilizando compreensao de listas
--(a) Gere uma lista dos quadrados dos numeros pares de 1 a 20.
quadrado = [x^2 | x <- [1..20], x `mod` 2 == 0]

--(b) Gere uma lista de todos os numeros impares de 1 a 50.
impares = [x | x <- [1..50], x `mod` 2 /= 0]

--(c) Gere uma lista de numeros de 1 a 100 que sao multiplos de 3 e 5.
multiplos3e5 = [x | x <- [1..100], (x `mod` 3 == 0) && (x `mod` 5 == 0)]

--(d) Gere uma lista de numeros de 1 a 100 que sao divisiveis por 7.
div7 = [x | x <- [1..100], x `mod` 7 == 0]

--(e) Gere uma lista de numeros palindromos de 1 a 1000. Um número palindromo eh aquele que eh igual ao seu reverso

-- 1, 2, 3, ..., 9
-- 11, 22, 33, ..., 99
-- 101, 111, 121, 131, ..., 191
-- 202, 212, 222, 232, ..., 292
-- ...
-- 909, 919, 929, 939, ..., 999

-- x é menor que 10
-- x é divisivel por 11
-- resto da divisao por 101 seja multiplo de 10 (e o resto da divisao por 101 tem que ser diferente de 100)

numerosPalindromos = [x | x <- [1..1000], (x < 10) || ((x < 100) && (x `mod` 11 == 0)) || ((((x `mod` 101) /= 100) && ((x `mod` 101) `mod` 10) == 0))]

-- 2. Utilizando funcoes de alta ordem map, foldl, foldr e filter
-- (a) Crie uma funcao que dobra cada elemento de uma lista
map' :: (a -> b) -> [a] -> [b]
map' f lista = [f x | x <- lista]

dobrar :: [Int] -> [Int]
dobrar lista = (map' (*2) lista)

-- (b) Escreva uma funcao que filtra apenas os números pares de uma lista de 1 a 20.
filter' :: (a -> Bool) -> [a] -> [a]
filter' f lista = [x | x <- lista, (f x) == True]

pares :: [Int] -> [Int]
pares lista = filter' even lista

-- (c) Use foldl para somar todos os elementos de uma lista
foldl' :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl' f v [] = v
foldl' f v (x:lista) = foldl' f (f v x) lista 

somar :: [Int] -> Int
somar lista = foldl' (+) 0 lista


-- (d) Combine map e filter para criar uma funcao que primeiro filtra os numeros pares e depois os dobra.

dobraPar :: [Int] -> [Int]
dobraPar lista = map (*2) (filter even lista)


-- (e) Use foldl para calcular o produto de todos os elementos de uma lista.

produto :: [Int] -> Int
produto lista = foldl (*) 1 lista

-- (f) Escreva uma função que primeiro filtra os números maiores que 5 e depois adiciona 1 a cada um deles.

sucessorMaiores5 :: [Int] -> [Int]
sucessorMaiores5 lista = map succ (filter (>5) lista)


--3. Implemente uma funcao polimorfica myAll que verica se todos os elementos de uma lista satisfazem um predicado.

-- myAll :: predicado -> [a] -> Bool 
myAll :: (a -> Bool) -> [a] -> Bool
myAll f [] = False
myAll f (x:[]) = if (f x) then True else False
myAll f (x:lista) = if f x then myAll f lista else False

--4. Crie as seguintes funcoes polimorficas
--(a) Inverter uma lista
rev :: [a] -> [a]
rev [] = []
rev (x:lista) = rev lista ++ [x]

--(b) Remover o último elemento da lista
rmvUltimo :: [a] -> [a]
rmvUltimo (x:[]) = []
rmvUltimo (x:lista) = x : (rmvUltimo lista) 

--(c) Obter o segundo elemento da lista
segundoLista :: [a] -> a
segundoLista (x:y:lista) = y

--6. Investigue o tipo e funcionamento da funcao concat em Haskell. Implemente essa funcao usando a funcao foldr.
concatenar :: [[a]] -> [a]
concatenar lista = foldr (++) [] lista

--7. Implemente a funcao mapish.
--ghci> mapish [(+1), (*3)] 10 
--[11, 30]

mapish :: [(a -> b)] -> a -> [b]
mapish (f:[]) x = (f x) : []
mapish (f:listaF) x = (f x) : (mapish listaF x)