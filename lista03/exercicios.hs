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

somarListaInteiros :: [Integer] -> Integer

dobrarListaInteiros :: [Integer] -> Integer

ehElementoListaInteiros :: [Integer] -> Bool

insertSort :: [Integer] -> Integer

selectionSort :: [Integer] -> Integer

mergeSort :: [Integer] -> Integer

quickSort :: [Integer] -> Integer