type Nome = String
type Preco = Int
type CodigoBarra = Int
type BancoDeDados = [ (CodigoBarra, Nome, Preco) ]

bd :: BancoDeDados
bd = [
  (1001, "Refrigerante", 450), 
  (1002, "Leite", 320), 
  (1003, "Biscoito", 200), 
  (1004, "Suco", 989),
  (1005, "Arroz", 345),
  (1006, "Feijao", 780)
  ]

retornaCodigoBarra :: (CodigoBarra, Nome, Preco) -> CodigoBarra
retornaCodigoBarra (codigoBarra, _, _) = codigoBarra

retornaNome :: (CodigoBarra, Nome, Preco) -> Nome
retornaNome (_, nome, _) = nome

retornaPreco:: (CodigoBarra, Nome, Preco) -> Preco
retornaPreco (_, _, preco) = preco

buscarBDaux :: CodigoBarra -> BancoDeDados -> (Nome, Preco)
buscarBDaux cod ((codigoBD, nomeBD, precoBD):restoBD)
  | cod == codigoBD = (nomeBD, precoBD)
  | otherwise = buscarBDaux cod restoBD

buscarBD :: CodigoBarra -> (Nome, Preco)
buscarBD cod = (buscarBDaux cod bd)

fazerConta :: [CodigoBarra] -> [(Nome, Preco)]
fazerConta (cod:[]) = ((buscarBD cod):[])
fazerConta (cod:resto) = ((buscarBD cod):(fazerConta resto))

dividir :: Int -> String
dividir num = (show(num `div` 100)) ++ "." ++ (show(num `mod` 100))

repetir :: Int -> String -> String
repetir 1 str = str 
repetir rep str = (str ++ (repetir (rep-1) str))

tamanhoLinha :: Int
tamanhoLinha = 30

formatarLinha :: (Nome, Preco) -> String
formatarLinha (nome, preco) =( nome ++ (repetir tamanhoLinha ".") ++ (dividir preco) ++ "\n") 

formatarLinhas :: [(Nome, Preco)] -> String
formatarLinhas ((nome, preco):[]) = (formatarLinha (nome, preco))
formatarLinhas ((nome,preco):resto) = ((formatarLinha (nome, preco)) ++ (formatarLinhas resto))

calcularTotal :: [(Nome, Preco)] -> Int
calcularTotal ((nome, preco):[]) = preco
calcularTotal ((nome, preco):resto) = (preco + (calcularTotal resto))

formatarTotal :: Int -> String
formatarTotal num = "Total:" ++ (repetir tamanhoLinha ".") ++ (dividir num) ++ "\n" 

formatarConta :: [(Nome, Preco)] -> String
formatarConta lista = (formatarLinhas lista) ++ (formatarTotal(calcularTotal lista))

imprimirConta :: [CodigoBarra] -> IO()
imprimirConta lista = putStr (formatarConta (fazerConta (lista)))