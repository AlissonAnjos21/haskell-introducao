-- Recursao (a maioria)

somarPares :: Integer -> Integer
somarPares x
    | x == 0 = 0
    | (x `mod` 2) == 0 = x + somarPares(x - 2)
    | (x `mod` 2) == 1 = somarPares(x - 1)

elevado :: Integer -> Integer -> Integer
elevado x y
    | y == 0 = 1
    | otherwise = x * (elevado x (y-1))

somarTodosNoIntervalo :: Int -> Int -> Int
somarTodosNoIntervalo a b
  | a == b = a
  | otherwise = b + (somarTodosNoIntervalo a (b-1))

somarDigitos :: Int -> Int
somarDigitos x
  | x >= 0 && x <= 9 = x 
  | otherwise = (x `mod` 10) + (somarDigitos (x `div` 10))

somarQuadrados :: Int -> Int
somarQuadrados x
  | x == 1 = 1
  | otherwise = (x*x) + (somarQuadrados (x-1)) 

fatorial :: Int -> Int
fatorial x
  | x == 0 = 1
  | otherwise = x * (fatorial (x-1))

somarFatoriais :: Int -> Int
somarFatoriais x
  | x == 0 = 1
  | otherwise = (fatorial x) + (somarFatoriais (x-1))