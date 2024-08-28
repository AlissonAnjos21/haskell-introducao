main :: IO()

ehDigito :: Char -> Bool
ehDigito d = (d >= '0') && (d <= '9')

main = print(ehDigito 'A')