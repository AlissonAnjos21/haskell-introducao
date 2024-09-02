doisAlgarismos :: Integer -> Integer
doisAlgarismos n = (n `mod` 10) + (n `div` 10)

tresAlgarismos :: Integer -> Integer
tresAlgarismos n = (n `div` 100) + (doisAlgarismos (n `mod` 100))

somar :: Integer -> Integer
somar n
  | (n >= 0 && n < 10) = n 
  | (n >= 10 && n < 100) = doisAlgarismos n
  | (n >= 100 && n < 1000) = tresAlgarismos n
  | otherwise = -1
