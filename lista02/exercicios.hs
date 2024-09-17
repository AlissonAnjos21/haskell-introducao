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

approxPi :: Integer -> Double 
approxPi n 
  | n == 0 = 4
  | otherwise = 4 * (fromIntegral((-1)^n) / (fromIntegral(2*n)+1)) + (approxPi (n-1))

somarPorSucessor :: Int -> Int -> Int
somarPorSucessor x y
  | y == 0 = x
  | otherwise = 1 + (somarPorSucessor x (y-1))

multiplicacaoPorSoma :: Int -> Int -> Int
multiplicacaoPorSoma x y
  | y == 0 = 0
  | otherwise = x + (multiplicacaoPorSoma x (y - 1))

quantidadeDigitos :: Int -> Int
quantidadeDigitos x
  | (x `div` 10) == 0 = 1
  | otherwise = 1 + (quantidadeDigitos (x `div` 10))

concatenarNvezes :: String -> Int -> String
concatenarNvezes palavra n 
  | n == 0 = palavra
  | otherwise = palavra ++ (concatenarNvezes palavra (n-1))

decimalParaBinario :: Int -> String
decimalParaBinario n 
  | (n `div` 2) == 0 || (n `div` 2) == 1 = show(n `div` 2) ++ show(n `mod` 2)
  | otherwise = (decimalParaBinario (n `div` 2)) ++ show(n `mod` 2)

converteParaListaDeInt :: Int -> [Int]
converteParaListaDeInt x = map(\x -> read[x] :: Int) (show x)

aparicoesKnoNaturalN :: Int -> Int -> Int
aparicoesKnoNaturalN n k = length( (filter(==k)) (converteParaListaDeInt n))

fibonacci :: Int -> Int
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fibonacci (n-1)) + (fibonacci (n-2))

fibonacciTail :: Int -> Int
fibonacciTail 0 = 0
fibonacciTail 1 = 1
fibonacciTail n = (fibonacciTail (n-1)) + (fibonacciTail (n-2))

mdc :: Int -> Int -> Int
mdc a b
  | b == 0 = a
  | otherwise = (mdc b (a `mod` b))

tabuadaN :: Int -> Int -> String
tabuadaN n m
  | m == 0 = (show 0)
  | otherwise = (show (n * m)) ++ " " ++ (tabuadaN n (m-1))

calcularNumeroEulerComNtermos :: Int -> Float
calcularNumeroEulerComNtermos n
  | n == 0 = 1
  | otherwise = (1 / fromIntegral (fatorial n)) + (calcularNumeroEulerComNtermos (n-1))

tabelaValoresNumeroEuler :: Int -> String
tabelaValoresNumeroEuler t
  | t == 1 = show (calcularNumeroEulerComNtermos 1)
  | t > 1 = show (calcularNumeroEulerComNtermos t) ++ " " ++ (tabelaValoresNumeroEuler (t-1))
  | otherwise = "Erro"

mediaQuantidadeN :: [Double] -> Double
mediaQuantidadeN valores = (sum valores) / fromIntegral (length valores)

desvioPadraoQuantidadeN :: [Double] -> Double
desvioPadraoQuantidadeN valores = sqrt( 
  (sum(
    (map (\xi -> (xi - media)^2) valores)
    ))
   / fromIntegral (length valores) )
  where
    media = (mediaQuantidadeN valores)


fatorialParaTaylor :: Integer -> Integer
fatorialParaTaylor 0 = 1
fatorialParaTaylor n = n * (fatorialParaTaylor (n-1))

taylorSin :: Double -> Integer -> Double
taylorSin x 0 = x
taylorSin x n = ((((-1)^n) * valor) / fromIntegral fatorial ) + (taylorSin x (n-1))
  where
    fatorial = fatorialParaTaylor (2 * n + 1)
    valor = x^(2 * n + 1)

taylorCos :: Double -> Integer -> Double
taylorCos x 0 = 1
taylorCos x n = ((((-1)^n) * valor) / fromIntegral fatorial ) + (taylorCos x (n-1))
  where
    fatorial = fatorialParaTaylor (2 * n)
    valor = x^(2 * n)