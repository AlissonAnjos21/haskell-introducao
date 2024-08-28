import Data.Char (chr, ord)

main :: IO()

minusculaParaMaiuscula :: Char -> Char
minusculaParaMaiuscula c
  | c >= 'a' && c <= 'z' = chr (ord c - 32)
  | otherwise = c


main = print(minusculaParaMaiuscula 'z')