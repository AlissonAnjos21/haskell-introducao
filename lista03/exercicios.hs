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

-- insertSort :: [Integer] -> [Integer]

-- selectionSort :: [Integer] -> [Integer]

-- mergeSort :: [Integer] -> [Integer]

-- quickSort :: [Integer] -> [Integer]

transformaEmLista ::  String -> [String]
transformaEmLista "" = []
transformaEmLista (a:x) = (show a):(transformaEmLista x)

contarVogaisString :: String -> Integer
contarVogaisString [] = 0
contarVogaisString (a:x) = if (a == 'a') || (a == 'e') || (a == 'i') || (a == 'o') || (a == 'u') || (a == 'A') || (a == 'E') || (a == 'I') || (a == 'O') || (a == 'U') then 1 + (contarVogaisString x) else 0 + (contarVogaisString x)

-- quantidadePalavras :: String -> Int

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

-- maiorElementoListaInteiros :: [Integer] -> Integer

-- menorElementoListaInteiros :: [Integer] -> Integer

-- maiorMenorElementoListaInteiros :: [Integer] -> (Integer, Integer)

-- quantidadeLetras

-- quantidadeLetrasPalavras :: String -> (Integer, Integer)

-- ehPalindroma :: String -> Bool
