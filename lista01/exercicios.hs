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
