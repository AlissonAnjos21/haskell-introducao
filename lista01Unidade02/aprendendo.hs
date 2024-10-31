-- funcao alta ordem (recebe 1 ou mais funcoes como argumento ou retorna uma funcao) 

aplicarDuasVezes :: (a -> a) -> a -> a 
-- uma funcao que recebe uma funcao (a -> a) e tambem recebe um valor a e retorna como resultado um valor a
aplicarDuasVezes f x = f (f x) -- recebo a funcao f e o valor x, e vou aplicar f uma segunda vez depois de ja ter aplicado

-- exemplo:
--ghci> reverse [1, 2, 3]
--[3,2,1]
--ghci> aplicarDuasVezes reverse [1, 2, 3]
--[1,2,3]

mapNaoFeitaPronta :: (a -> b) -> [a] -> [b]
mapNaoFeitaPronta f lista = [(f elementoLista) | elementoLista <- lista]

mapNaoFeitaProntaRecursiva :: (a -> b) -> [a] -> [b]
mapNaoFeitaProntaRecursiva f [] = []
mapNaoFeitaProntaRecursiva f (x:lista) = (f x) : (mapNaoFeitaProntaRecursiva f lista)

filterNaoFeitoPronto :: (a -> Bool) -> [a] -> [a]
filterNaoFeitoPronto f lista = [elemento | elemento <- lista, (f elemento) == True]

filterNaoFeitoProntoRecursivo :: (a -> Bool) -> [a] -> [a]
filterNaoFeitoProntoRecursivo f [] = []
filterNaoFeitoProntoRecursivo f (x:lista) = if (f x) == True then x : filterNaoFeitoProntoRecursivo f lista else filterNaoFeitoProntoRecursivo f lista

{-
foldr :: (a -> b -> b) -> b -> [a] -> b

ou seja:
        recebe uma funcao (a -> b -> b), funcao essa que recebe como parametros a e b e retorna um valor b;
        tambem recebe um valor b;
        tambem recebe uma lista [a];
        retorna um valor b

sua aplicacao eh realizada da seguinte forma:

a1 `f` (a2 `f` (a3 `f` v) )

exemplo:
funcao soma:

foldr (+) 0 [1, 2, 3] = (+) (1 ((+) 2 ((+) 3 ((+) 0) )  )   )

obs: o segundo parametro eh zero, pois ele eh o elemento neutro da adicao
-}

--foldl :: (a -> b -> a) -> a -> [b] -> a

{-
topicos para aprofundamento:
  *lambda calculo
  *funcoes polimorficas
  *map, foldl, foldr e filter
-}

{-
  foldl: funciona da esquerda pra direita (por isso, pode ser lido como foldLEFT)
  (b -> a -> b) -> b -> [a] -> b

  (b -> a -> b): funcao acumuladora - eh uma funcao que recebe um acumulador inicial b, um valor a e retorna um novo acumulador b
  
  b: acumulador inicial

  [a]: lista de valores que serao passados para a funcao acumuladora
  
  b: resultado final - resultado apos todas as acumulacoes
-}

foldEsquerda :: (b -> a -> b) -> b -> [a] -> b
foldEsquerda f v [] = v
foldEsquerda f v (x:xs) = foldEsquerda f (f v x) xs

{-
exemplo:
foldEsquerda (+) 0 [1, 2, 3, 4]
foldEsquerda (+) ((+) 0 1) [2, 3, 4]
foldEsquerda (+) ((+) ((+) 0 1) 2) [3, 4]
foldEsquerda (+) ((+) ((+) ((+) 0 1) 2) 3) [4]
foldEsquerda (+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) []
((+) ((+) ((+) ((+) 0 1) 2) 3) 4)
((+) ((+) ((+) 1 2) 3) 4)
((+) ((+) 3 3) 4)
((+) 6 4)
10
-}

{-
a funcao foldr eh semelhante, porem o processamento ocorre da direita para a esquerda
(a -> b ->  b) -> b -> [a] -> b
-}

foldDireita :: (a -> b -> b) -> b -> [a] -> b
foldDireita f v [] = v
foldDireita f v (x:xs) =  f x (foldDireita f v xs)

-- o foldr eh melhor para quando trabalhamos com listas infinitas pois, ele fornece resultado antes mesmo de terminar de percorrer a lista. Lembre-se que o Haskell trabalha com lazy evaluation.
