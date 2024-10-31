--Tipos Algebricos

--tipos soma:
data Animal = Macaco | Vaca | Porco 
-- algo do tipo Animal so pode assumir apenas um dentre esses tres valores


--tipos produto:
type Marca = String
type Ano = Int

data Carro = Carro Marca Ano
-- algo do tipo Carro assume todos esses atributos simultaneamente
-- o "Carro" no lado direito da igualdade corresponde ao construtor


-- Misturando ambos (Tipos Soma e Tipos Produto):
-- Varios construtores e campos:
data Forma = Circulo Float
          | Retangulo Float Float
          | Triangulo Float Float

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo l1 l2) = l1 * l2
area (Triangulo b h) = (b * h)/2



-- Tipos Algebricos Recursivos (uteis para criar estruturas de dados)
data Lista a = Empty | Cons a (Lista a)
-- [1, 2, 3] = Cons 1 (Cons 2 (Cons 3 Empty))
-- Aprofundarei mais na utilidade disso quando for tratar de Tipos Abstratos de Dados




-- Tipo Parametrico Maybe
data Maybe' a = Nothing' | Just' a
                deriving Show --me possibilita imprimir (transformar em string) isso

--uso pratico:
divisaoSegura :: Float -> Float -> Maybe' Float
divisaoSegura _ 0 = Nothing'
divisaoSegura x y = Just' (x/y)

divisaoSegura2 :: Float -> Float -> Maybe Float
divisaoSegura2 _ 0 = Nothing
divisaoSegura2 x y = Just (x/y)



data Either' a b = Left' a | Right' b
                  deriving Show

divisaoComErro :: Int -> Int -> Either' String Int
divisaoComErro _ 0 = Left' "Por zero nao neh"
divisaoComErro x y = Right' (x `div` y) 
