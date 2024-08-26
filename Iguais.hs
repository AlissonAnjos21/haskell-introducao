main :: IO ()

saoIguais :: Integer -> Integer -> Integer -> Integer -> Bool
saoIguais a b c d 
  | a == b && a == c && a == d = True
  | otherwise = False

main = print(saoIguais 21 21 21 21)