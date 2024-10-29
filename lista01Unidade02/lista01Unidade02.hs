--1. Utilizando compreensão de listas
--(a) Gere uma lista dos quadrados dos números pares de 1 a 20.
quadrado = [x^2 | x <- [1..20], x `mod` 2 == 0]

--(b) Gere uma lista de todos os números ímpares de 1 a 50.
impares = [x | x <- [1..50], x `mod` 2 /= 0]

--(c) Gere uma lista de números de 1 a 100 que são múltiplos de 3 e 5.
multiplos3e5 = [x | x <- [1..100], (x `mod` 3 == 0) && (x `mod` 5 == 0)]

--(d) Gere uma lista de números de 1 a 100 que são divisíveis por 7.
div7 = [x | x <- [1..100], x `mod` 7 == 0]

--(e) Gere uma lista de números palíndromos de 1 a 1000. Um número palíndromo é aquele que é igual ao seu reverso

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

-- 2. Utilizando funções de alta ordem map, foldl, foldr e filter
-- (a) Crie uma função que dobra cada elemento de uma lista
-- (b) Escreva uma função que filtra apenas os números pares de uma lista de 1 a 20.
-- (c) Use foldl para somar todos os elementos de uma lista
-- (d) Combine map e filter para criar uma função que primeiro ltra os números pares e depois os dobra.
-- (e) Use foldl para calcular o produto de todos os elementos de uma lista.
-- (f) Escreva uma função que primeiro filtra os números maiores que 5 e depois adiciona 1 a cada um deles.

