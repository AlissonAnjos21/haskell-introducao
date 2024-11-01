--1) Considere os seguintes tipos de dados utilizados para a representacao de expressoes aritmeticas por arvores binarias:
data OP = SOMA | SUB | PROD | DIV deriving (Show, Eq)
data Expr = Folha Int | Nodo OP Expr Expr deriving (Show, Eq)
--a) Escreva uma funcao aplica que aplica um operador binario a dois argumentos inteiros:



--b) Escreva uma funcao avalia que procede ao calculo do valor de uma expressao:


--c) Escreva finalmente uma funcao imprime que produz uma string com a representacao usual de uma expressao representada por uma arvore:





--2) Considere as seguintes declaracoes de tipo usadas para representar as horas de um dia nos formatos usuais.
data Part = AM | PM
          deriving (Eq, Show)

data TIME = Local Int Int Part -- Formato em 12 horas
          | Total Int Int -- Formato em 24 horas

time1 :: TIME
time1 = Local 3 45 PM -- 3:45 PM

time2 :: TIME
time2 = Local 11 30 AM -- 11:30 AM

time3 :: TIME
time3 = Total 15 20 -- 15:20 (3:20 PM)

time4 :: TIME
time4 = Total 9 5 -- 09:05 AM

--a. Defina a funcao totalMinutos :: TIME -> Int que conta o total de minutos de uma dada hora.
totalMinutos :: TIME -> Int
totalMinutos (Total h m) = (h * 60) + m 
totalMinutos (Local h m AM) = (h * 60) + m
totalMinutos (Local h m PM) = (12 * 60) + (h * 60) + m

--b. Defina TIME como instancia da classe Eq de forma a que a igualdade entre horas seja independente do formato em que hora esta guardada.

instance Eq TIME where
  (Local h1 m1 AM) == (Local h2 m2 AM) = (h1 == h2) && (m1 == m2) && (AM == AM)
  (Local h1 m1 AM) == (Total h2 m2) = (h2 < 12) && (h1 == h2) && (m1 == m2)
  (Local h1 m1 PM) == (Local h2 m2 PM) = (h1 == h2) && (m1 == m2) && (PM == PM)
  (Local h1 m1 PM) == (Total h2 m2) = (h2 > 12) && ((h1+12) == h2) && (m1 == m2)
  (Total h1 m1) == (Total h2 m2) = (h1 == h2) && (m1 == m2)

--c. Defina TIME como instancia da classe Ord.

{-
instance Ord TIME where
  (Local h1 m1 AM) < (Local h2 m2 AM) =
  (Local h1 m1 AM) <= (Local h2 m2 AM) =   
  (Local h1 m1 AM) > (Local h2 m2 AM) =
  (Local h1 m1 AM) >= (Local h2 m2 AM) =
  
  (Local h1 m1 PM) < (Local h2 m2 PM) =
  (Local h1 m1 PM) <= (Local h2 m2 PM) =   
  (Local h1 m1 PM) > (Local h2 m2 PM) =
  (Local h1 m1 PM) >= (Local h2 m2 PM) =

  (Total h1 m1) < (Total h2 m2) =
  (Total h1 m1) <= (Total h2 m2) =   
  (Total h1 m1) > (Total h2 m2) =
  (Total h1 m1) >= (Total h2 m2) = 

  (Local h1 m1 AM) < (Total h2 m2) =
  (Local h1 m1 AM) <= (Total h2 m2) =
  (Local h1 m1 AM) > (Total h2 m2) =
  (Local h1 m1 AM) >= (Total h2 m2) =

  (Local h1 m1 PM) < (Total h2 m2) =
  (Local h1 m1 PM) <= (Total h2 m2) =
  (Local h1 m1 PM) > (Total h2 m2) =
  (Local h1 m1 PM) >= (Total h2 m2) =

--nao vou fazer pela falta de tempo, mas nao eh algo complicado
-}




--3) Dado o tipo algebrico:

data Nat = Zero | Succ Nat --deriving (Show)

{-
Esse tipo representa numeros naturais, onde:
- Zero eh o valor base (equivalente ao numero 0).

- Succ n representa o sucessor de um numero natural (por exemplo, Succ Zero eh 1, Succ (Succ Zero)
eh 2, e assim por diante).
Implemente as seguintes funcoes para operar com valores do tipo Nat:
-}

--a. Funcao para converter Nat para Int:
--A funcao deve converter um numero natural no tipo Nat para um valor do tipo Int.
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ x) = 1 + (natToInt x)

--b. Funcao para somar dois numeros naturais:
--A funcao deve receber dois numeros naturais e retornar a sua soma, tambem no tipo Nat.
soma :: Nat -> Nat -> Nat
soma Zero y = y
soma (Succ x) y = soma x (Succ y)

--c. Incluir o tipo Nat na classe Eq e na classe Show

instance Eq Nat where
  (Zero) == (Zero) = True
  (Succ n1) == (Succ n2) = (natToInt n1) == (natToInt n2) 

instance Show Nat where
  show n = show (natToInt n)