type Matricula = Int
type Nome = String
type Titulacao = String
type Sexo = Char

banco :: Int -> (Matricula,Nome,Titulacao,Sexo)
banco matricula
  | matricula == 1 = (1,"Roque","Doutor",'M')
  | matricula == 2 = (2,"Alzira","Doutor",'F')
  | matricula == 3 = (3,"Helio","Doutor",'M')
  | matricula == 4 = (4,"Maisa","Doutor",'F')
  | matricula == 5 = (5,"Carlos","Mestre",'M')
  | matricula == 6 = (6,"Rita","Mestre",'F')
  | otherwise = (0,"","",' ')

--numeroDoutores

--numeroMulheres

--numeroMulheresSaoMestres

--nomesDoutores