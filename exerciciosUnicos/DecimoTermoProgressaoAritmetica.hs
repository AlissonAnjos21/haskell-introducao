main :: IO()

decimoPA :: Float -> Float -> Float
decimoPA termo1 razao = termo1 + 9*razao

main = print(decimoPA 10 5)