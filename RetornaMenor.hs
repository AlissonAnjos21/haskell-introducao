main :: IO ()

menor :: Integer -> Integer -> Integer -> Integer
menor x y z
  | x <= y && x <= z = x
  | y <= x && y <= z = y 
  | z <= x && z <= y = z

main = print(menor 21 42 3)
