import Distribution.Simple.Utils (xargs)
main :: IO()

somar :: Integer -> Integer -> Integer
somar x
  | (x >= 0) && (x <= 9) = x
  |  (x > 9) && (x <= 99) =
