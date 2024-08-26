main :: IO ()

type Base = Float
type Altura = Float
type Area = Float

area :: Base -> Altura -> Area
area b h = (b*h)/2.0

main = print(area 1 1)
