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