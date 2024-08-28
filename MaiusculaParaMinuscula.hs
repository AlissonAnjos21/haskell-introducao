import Data.Char (chr, ord)

main :: IO()

maiusculaParaMinuscula :: Char -> Char
maiusculaParaMinuscula c
  | c >= 'A' && c <= 'Z' = chr (ord c + 32)
  | otherwise = c


main = print(maiusculaParaMinuscula 'A')