main :: IO ()

saoDiferentes :: Integer -> Integer -> Integer -> Bool
saoDiferentes a b c
  | a /= b && a /= c && b /= c = True
  | otherwise = False

main = print(saoDiferentes 21 1 3)