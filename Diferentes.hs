main :: IO ()

saoIguais :: Integer -> Integer -> Integer -> Bool
saoIguais a b c
  | a /= b && a /= c && b /= c = True
  | otherwise = False

main = print(saoIguais 21 1 3)