main :: IO()

ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo p q = (p && not q) || (not p && q)

main = print(ouExclusivo False True)