main :: IO()

calculaNand :: Bool -> Bool -> Bool
calculaNand p q
  | not (p && q) = True
  | otherwise = False

main = print(calculaNand True True)  