import Data.Char (chr, ord)
mediaNotas :: Float -> Float -> Float -> Float
mediaNotas n1 n2 n3 = (n1 + n2 + n3)/3

podeSerTriangulo :: Float -> Float -> Float -> Bool
podeSerTriangulo x y z
  | ((x + y) > z) && ((y + z) > x) && ((x + z) > y) = True
  | otherwise = False

tipoTriangulo :: Float -> Float -> Float -> String
tipoTriangulo x y z
  | ((x == 90) || (y == 90) || (z == 90)) && (((x + y + z) > 0) && (x + y + z) <= 180 ) = "Retangulo"
  | ((x > 90) || (y > 90) || (z > 90)) && (((x + y + z) > 0) && (x + y + z) <= 180 ) = "Obtusangulo"
  | ((x < 90) || (y < 90) || (z < 90)) && (((x + y + z) > 0) && (x + y + z) <= 180 ) = "Acutangulo"
  | otherwise = "Nao eh triangulo"

calcularTermon :: Float -> Float -> Float -> Float 
calcularTermon termo1 n passo = termo1 + ((n-1)*passo) 

somaPA :: Float -> Float -> Float -> Float
somaPA termo1 passo n = ((termo1 + (calcularTermon termo1 n passo)) * n)/2
-- somaPA termo1 termon n = ((termo1 + termon)*n)/2

posicaoPlanoCartesiano :: Float -> Float -> String
posicaoPlanoCartesiano x y 
  | x == 0 && y == 0 = "Origem"
  | x > 0 && y == 0 = "Eixo X positivo"
  | x < 0 && y == 0 = "Eixo X negativo"
  | x == 0 && y > 0 = "Eixo Y positivo"
  | x == 0 && y < 0 = "Eixo Y negativo"
  | x > 0 && y > 0 = "Primeiro quadrante"
  | x < 0 && y > 0 = "Segundo quadrante"
  | x < 0 && y < 0 = "Terceiro quadrante"
  | x > 0 && y < 0 = "Quarto quadrante"


mes :: Int -> String
mes n
  | n < 1 || n > 12 = "Nao eh mes"
  | n == 1 = "Janeiro"
  | n == 2 = "Fevereiro"
  | n == 3 = "Marco"
  | n == 4 = "Abril"
  | n == 5 = "Maio"
  | n == 6 = "Junho"
  | n == 7 = "Julho"
  | n == 8 = "Agosto"
  | n == 9 = "Setembro"
  | n == 10 = "Outrubro"
  | n == 11 = "Novembro"
  | n == 12 = "Dezembro"

ehPar :: Int -> Bool
ehPar x
  | x `mod`2 == 0 = True
  | otherwise = False

numeroRaizesEquacao2Grau :: Float -> Float -> Float -> Int
numeroRaizesEquacao2Grau a b c
  | ((b * b) - (4 * a * c)) > 0 = 2
  | ((b * b) - (4 * a * c)) == 0 = 1
  | otherwise = 0

fahrenheitParaCelsius :: Float -> Float
fahrenheitParaCelsius f = (f - 32)/1.8

imc :: Float -> Float -> Float
imc peso altura = peso/(altura * altura)

proximaLetra :: Char -> Char
proximaLetra letra 
  | letra == 'Z' = 'A'
  | letra == 'z' = 'a'
  | otherwise = chr (ord letra+1)