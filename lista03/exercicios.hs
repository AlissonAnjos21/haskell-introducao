import Data.Char (chr, ord)

type Ponto = (Float, Float)
distancia :: Ponto -> Ponto -> Float
distancia (x1, y1) (x2, y2) = sqrt(((x1-x2)^2) + ((y1-y2)^2))

type Centro = Ponto
type Raio = Float
type Circulo = (Centro, Raio)

estaNoCirculo :: Ponto -> Circulo -> Bool
estaNoCirculo (x, y) ((xc, yc), r) = if ((distancia (x, y) (xc, yc)) <= (r*r)) then True else False

minmax :: Int -> Int -> Int -> (Int, Int)
minmax x y z
  | (x < y && x < z) && y < z = (x, z)
  | (x < y && x < z) && z < y = (x, y)
  | (y < x && y < z) && x < z = (y, z)
  | (y < x && y < z) && z < x = (y, x)
  | (z < x && z < y) && y < x = (z, x)
  | (z < x && z < y) && x < y = (z, y)

maxocorre :: Int -> Int -> Int -> (Int, Int)
maxocorre x y z
  | (x <= y && y<= z) = if ((x == y) && (y == z)) then (z, 3) else if ((x == z) || (y == z)) then (z, 2) else (z, 1)
  | (y <= z && z<= x) = if ((x == y) && (y == z)) then (x, 3) else if ((y == x) || (z == x)) then (x, 2) else (x, 1)
  | (x <= z && z<= y) = if ((x == y) && (y == z)) then (y, 3) else if ((x == y) || (z == y)) then (y, 2) else (y, 1)

tamanhoListaInteiros :: [Integer] -> Integer
tamanhoListaInteiros [] = 0
tamanhoListaInteiros (a:x) = 1 + (tamanhoListaInteiros x)

somarListaInteiros :: [Integer] -> Integer
somarListaInteiros [] = 0
somarListaInteiros (a:x) = a + (somarListaInteiros x)

dobrarListaInteiros :: [Integer] -> [Integer]
dobrarListaInteiros [] = []
dobrarListaInteiros (a:x) = (2*a):(dobrarListaInteiros x) 

ehElementoListaInteiros :: Integer -> [Integer] -> Bool
ehElementoListaInteiros n [] = False
ehElementoListaInteiros n (a:x) = if (a == n) then True else (ehElementoListaInteiros n x)

inserir :: Integer -> [Integer] -> [Integer]
inserir a [] = [a]
inserir a (b:x)
  | a < b = (a:b:x)
  | otherwise = b:(inserir a x)

insertSort :: [Integer] -> [Integer]
insertSort [] = []
insertSort (a:x) = (inserir a (insertSort x))

minimoLocal :: Integer -> Integer -> Integer
minimoLocal x y = if (x < y) then x else y 

encontrarMinimo :: [Integer] -> Integer
encontrarMinimo (a:[]) = a
encontrarMinimo (a:x) = (minimoLocal a (encontrarMinimo x))

listaSemElementoMinimo :: [Integer] -> [Integer]
listaSemElementoMinimo [] = []
listaSemElementoMinimo (a:x) = if (a == (encontrarMinimo (a:x))) then x else (a:(listaSemElementoMinimo x))

selectionSort :: [Integer] -> [Integer]
selectionSort [] = []
selectionSort (a:x) = ((encontrarMinimo (a:x)):(selectionSort (listaSemElementoMinimo (a:x))))

-- mergeSort :: [Integer] -> [Integer]

maximoLocal :: Integer -> Integer -> Integer
maximoLocal x y = if (x > y) then x else y 

particiona :: Integer -> [Integer] -> ([Integer], [Integer])
particiona pivo [] = ([], [])
particiona pivo (a:x)
  |  pivo < a = (menores, a:maiores)
  | otherwise = (a:menores, maiores)
  where
    (menores, maiores) = (particiona pivo x) 

quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (pivo:x) = (quickSort(menores)) ++ [pivo] ++ (quickSort(maiores))
  where
    (menores, maiores) = (particiona pivo x)

transformaEmLista ::  String -> [String]
transformaEmLista "" = []
transformaEmLista (a:x) = (show a):(transformaEmLista x)

contarVogaisString :: String -> Integer
contarVogaisString [] = 0
contarVogaisString (a:x) = if (a == 'a') || (a == 'e') || (a == 'i') || (a == 'o') || (a == 'u') || (a == 'A') || (a == 'E') || (a == 'I') || (a == 'O') || (a == 'U') then 1 + (contarVogaisString x) else 0 + (contarVogaisString x)

quantidadePalavras :: String -> Integer
quantidadePalavras [] = 1
quantidadePalavras (a:x) = if (a == ' ') then 1 + (quantidadePalavras x) else 0 + (quantidadePalavras x)

cifraDeCesar :: Char -> Char
cifraDeCesar p
  | p == 'x' = 'a'
  | p == 'y' = 'b'
  | p == 'z' = 'c'
  | p == 'X' = 'A'
  | p == 'Y' = 'B'
  | p == 'Z' = 'C'
  | otherwise = chr((ord p) + 3)

palavraParaCifraDeCesar :: String -> String
palavraParaCifraDeCesar [] = []
palavraParaCifraDeCesar (a:x) = (cifraDeCesar a):(palavraParaCifraDeCesar x)

inverterString :: String -> String
inverterString [] = []
inverterString (a:x) = (inverterString x) ++ [a]

-- minusculoParaMaiusculo :: Char -> Char
-- minusculoParaMaiusculo p = chr((ord p) - 32)

-- minusculasParaPrimeiraPalavraMaiuscula :: String -> String

-- buscaBinaria ::

maiorElementoListaInteiros :: [Integer] -> Integer
maiorElementoListaInteiros (a:[]) = a 
maiorElementoListaInteiros (a:b:x) = if (a > b) then (maiorElementoListaInteiros (a:x)) else (maiorElementoListaInteiros (b:x))  

menorElementoListaInteiros :: [Integer] -> Integer
menorElementoListaInteiros (a:[]) = a 
menorElementoListaInteiros (a:b:x) = if (a < b) then (menorElementoListaInteiros (a:x)) else (menorElementoListaInteiros (b:x))

maiorMenorElementoListaInteiros :: [Integer] -> (Integer, Integer)
maiorMenorElementoListaInteiros lista = (maior, menor)
  where
    maior = (maiorElementoListaInteiros lista)
    menor = (menorElementoListaInteiros lista)

quantidadeLetras :: String -> Integer
quantidadeLetras [] = 0
quantidadeLetras (a:x) = if (a == ' ') then 0 + (quantidadeLetras x) else 1 + (quantidadeLetras x)

quantidadeLetrasPalavras :: String -> (Integer, Integer)
quantidadeLetrasPalavras str = (letras, palavras)
  where
    letras = (quantidadeLetras str)
    palavras = (quantidadePalavras str)

ehPalindroma :: String -> Bool
ehPalindroma str = if (str == (inverterString str)) then True else False

-- nomes :: String -> [String]
-- nomes [] = []
-- nomes (a:x) = 

-- nomeCompletoComSubnomesAbreviados :: String -> String

-- divisoresDeN :: Integer -> [Integer]
-- a opcao abaixo funciona porem usa de ferramentas ja presentes na linguagem
-- divisoresDeN n = [x | x <- [1..(n-1)], ((n `mod` x) == 0)]
-- faremos o mesmo so que de maneira direta
funcaoAuxiliarDivisoresDeN :: Integer -> Integer -> [Integer]
funcaoAuxiliarDivisoresDeN n 1 = [1]
funcaoAuxiliarDivisoresDeN n m = if ((n `mod`m) == 0) then m:(funcaoAuxiliarDivisoresDeN n (m-1)) else (funcaoAuxiliarDivisoresDeN n (m-1))

divisoresDeN :: Integer -> [Integer]
divisoresDeN n = (funcaoAuxiliarDivisoresDeN n (n-1))

ehNumeroPerfeito :: Integer -> Bool
ehNumeroPerfeito n = if((sum (divisoresDeN n)) == n) then True else False

-- intercalaListasOrdenadas :: [Integer] -> [Integer] -> [Integer]
-- intercalaListasOrdenadas (a:x) (b:y)
  -- | ((a:x) == (a:[])) || ((b:y) == (b:[])) = if (a > b)

temMaisDezElementos :: [Integer] -> Bool
temMaisDezElementos [] = False
temMaisDezElementos (_:_:_:_:_:_:_:_:_:_:_:x) = True
temMaisDezElementos _ = False

somarElementosListaInteirosTailRecursion :: [Integer] -> Integer
somarElementosListaInteirosTailRecursion [] = 0
somarElementosListaInteirosTailRecursion (a:x) = a + (somarElementosListaInteirosTailRecursion (x))
