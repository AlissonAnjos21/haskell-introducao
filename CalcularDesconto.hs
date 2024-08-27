import Distribution.Simple.Utils (xargs)
main :: IO()

calcularDesconto :: Float -> Float
calcularDesconto x = x - (x*0.2)

main = print(calcularDesconto(100))

