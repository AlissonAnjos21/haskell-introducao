type Matricula = Int
type Nome = String
type Titulacao = String
type Sexo = Char
type Banco = (Matricula,Nome,Titulacao,Sexo)

banco :: Int -> (Matricula,Nome,Titulacao,Sexo)
banco matricula
  | matricula == 1 = (1,"Roque","Doutor",'M')
  | matricula == 2 = (2,"Alzira","Doutor",'F')
  | matricula == 3 = (3,"Helio","Doutor",'M')
  | matricula == 4 = (4,"Maisa","Doutor",'F')
  | matricula == 5 = (5,"Carlos","Mestre",'M')
  | matricula == 6 = (6,"Rita","Mestre",'F')
  | otherwise = (0,"","",' ')

bancoLista :: [Banco]
bancoLista = [(banco 1), (banco 2), (banco 3), (banco 4), (banco 5), (banco 6)]

retornaMatricula :: Banco -> Matricula
retornaMatricula (x, _, _, _) = x

retornaNome :: Banco -> Nome
retornaNome (_, x, _, _) = x

retornaTitulacao :: Banco -> Titulacao
retornaTitulacao (_, _, x, _) = x

retornaSexo :: Banco -> Sexo
retornaSexo (_, _, _, x) = x

numeroDoutores :: [Banco] -> Integer
numeroDoutores (a:x)
  | (x == [])  = if ((retornaTitulacao a) == "Doutor") then 1 else 0 
  | otherwise = if ((retornaTitulacao a) == "Doutor") then 1 + (numeroDoutores x) else 0 + (numeroDoutores x)

numeroMulheres :: [Banco] -> Integer
numeroMulheres (a:[]) = if ((retornaSexo a) == 'F') then 1 else 0
numeroMulheres (a:x) = if ((retornaSexo a) == 'F') then 1 + (numeroMulheres x) else 0 + (numeroMulheres x)

numeroMulheresSaoMestres :: [Banco] -> Integer
numeroMulheresSaoMestres (a:[]) = if ((retornaSexo a) == 'F') && ((retornaTitulacao a) == "Mestre") then 1 else 0
numeroMulheresSaoMestres (a:x) = if ((retornaSexo a) == 'F') && ((retornaTitulacao a) == "Mestre") then 1 + (numeroMulheresSaoMestres x) else 0 + (numeroMulheresSaoMestres x)

nomesDoutores :: [Banco] -> [Nome]
nomesDoutores (a:[]) = if ((retornaTitulacao a) == "Doutor") then [(retornaNome a)] else []
nomesDoutores (a:x) = if ((retornaTitulacao a) == "Doutor") then [(retornaNome a)] ++ (nomesDoutores x) else [] ++ (nomesDoutores x)