main :: IO ()

mediaPonderada :: Float -> Float -> Float -> Float -> Float
mediaPonderada a b c d = (a + b*2 + c*3 + d*4)/10

main = print(mediaPonderada 1 2 3 4)