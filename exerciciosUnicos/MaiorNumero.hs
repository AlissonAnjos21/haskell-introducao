-- Calcular o maior de dois numeros inteiros

maior :: Int -> Int -> Int

maior a b | a >= b = a
          | b > a = b
          