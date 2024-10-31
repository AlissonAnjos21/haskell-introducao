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


--b. Defina TIME como instancia da classe Eq de forma a que a igualdade entre horas seja independente do formato em que hora esta guardada.


--c. Defina TIME como instancia da classe Ord.













--3) Dado o tipo algebrico:

data Nat = Zero | Succ Nat deriving (Show)

{-
Esse tipo representa numeros naturais, onde:
- Zero eh o valor base (equivalente ao numero 0).

- Succ n representa o sucessor de um numero natural (por exemplo, Succ Zero eh 1, Succ (Succ Zero)
eh 2, e assim por diante).
Implemente as seguintes funcoes para operar com valores do tipo Nat:
-}

--a. Funcao para converter Nat para Int:
natToInt :: Nat -> Int
natToInt x = 1
--A funcao deve converter um numero natural no tipo Nat para um valor do tipo Int.







--b. Funcao para somar dois numeros naturais:
soma :: Nat -> Nat -> Nat
soma x y = x
--A funcao deve receber dois numeros naturais e retornar a sua soma, tambem no tipo Nat.






--c. Incluir o tipo Nat na classe Eq e na classe Show


