main :: IO()

oqEh :: Float -> String
oqEh x
  | x > 0 = "Positivo"
  | x < 0 = "Negativo"
  | otherwise = "Nulo"

main = print(oqEh (-21))