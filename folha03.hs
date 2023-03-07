{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
{-# HLINT ignore "Redundant bracket" #-}
import GHC.Integer.GMP.Internals (powModInteger)
import GHC.Natural (powModNatural)
{-
Defina uma função divprop ∶∶ Integer → [Integer ] usando uma lista em
compreensão para calcular a lista de divisores próprios de um inteiro positivo
(i.e. inferiores ao número dado). Exemplo: divprop 10 = [1, 2, 5].
-}

divprop :: Integer -> [Integer]
divprop n = [x | x <- [1 .. n-1], mod n x == 0]

{-
Um inteiro positivo n diz-se perfeito se for igual à soma dos seus divisores
(excluindo o próprio n). Defina uma função perfeitos ∶∶ Integer → [Integer ]
que calcula a lista de todos os números perfeitos até um limite dado como
argumento. Exemplo: perfeitos 500 = [6, 28, 496]. Sugestão: utilize a solução
do exercício 3.1.
-}

perfeitos :: Integer -> [Integer]
perfeitos n = [x | x <- [1 .. n-1], x == sum (divprop x)]

{-
Um trio (x, y, z) de inteiros positivos diz-se pitagórico se x2 + y2 = z2. Defina a função 
pitagoricos ∶∶ Integer → [(Integer , Integer , Integer )] que calcule
todos os trios pitagóricos cujas componentes não ultrapassem o argumento. Por
exemplo: pitagoricos 10 = [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)].
-}

pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x /= y, x /= z, x^2 + y^2 == z^2]

{-
Defina uma função primo ∶∶ Integer → Bool que testa primalidade: n é
primo se tem exactamente dois divisores, a saber, 1 e n. Sugestão: utilize a
função do exercício 3.1 para obter a lista dos divisores próprios.
-}

primo :: Integer -> Bool
primo n = length (divprop n) == 1

{-
Usando uma função binom da folha 1 que calcula coeficientes binomiais,
escreva uma definição da função pascal ∶∶ Integer → [[Integer ]] que calcula o
triângulo de Pascal até à linha n
-}

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` 
            (product [product [1..k], product [1..(n-k)]])

pascal :: Integer -> [[Integer]]
pascal n = [[]]

{-
Podemos representar uma relação binária em conjuntos de inteiros como
um par Rel = ([Int], [(Int,Int)]); o primeiro elemento do par é a lista dos
inteiros no conjunto; o segundo elemento do par é a lista de pares na relação.
(a) Defina uma função reflexiva :: Rel -> Bool que verifica se a relação
(V, R) é reflexiva, isto é, se (x, x) ∈ R para todo x ∈ V .
(b) Defina uma função simetrica :: Rel -> Bool que verifica se a relação
(V, R) é simétrica, isto é, se (x, y) ∈ R ⇒ (y, x) ∈ R.
(c) Defina uma função transitiva :: Rel -> Bool que verifica se a rela-
ção (V, R) é transitiva, isto é, se (x, y) ∈ R ∧ (y, z) ∈ R ⇒ (x, z) ∈ R.
Sugestão: cada uma destas funções se podem definir apenas numa linha usando
listas em compreensão e funções do Prelúdio.
-}

reflexiva :: ([Int], [(Int, Int)]) -> Bool
reflexiva (v, [(n, m)]) = v == [x | x <- v, x == n, x == m]

{-

-}

{-

-}

{-

-}

{-

-}

{-

-}

{-

-}