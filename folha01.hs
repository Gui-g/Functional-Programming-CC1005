{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use splitAt" #-}
{-# HLINT ignore "Use last" #-}
{-# HLINT ignore "Use !!" #-}
{-# HLINT ignore "Use init" #-}

{-
    Comentários.
-}

{-
1.1 Considere as seguintes definições de funções:

incr, triplo :: Integer -> Integer
incr x = x+1
triplo x = 3*x
boasVindas :: String -> String
boasVindas nome = "Olá, " ++ nome ++ "!"

Smplifique as expressões seguintes o máximo possível efectuando reduções 
passo-a-passo. Pode verificar os resultados comparando-os com as respostas 
do interpretador.

(a) incr (triplo 3)

incr 3*3
incr 9
9 + 1
10

(b) triplo (incr 3)

triplo 3+1
triplo 4
3*4
12

(c) boasVindas "Linguagem" ++ " Haskell"

Olá, Linguagem! ++ "Haskell"
Olá, Linguagem! Haskell

(d) boasVindas ("Linguagem" ++ " Haskell")

Olá, ("Linguagem" ++ " Haskell")!
Olá, Linguagem Haskell!

(e) boasVindas (boasVindas "Haskell")

boasVindas Olá Haskell!
Olá, Olá, Haskell!!
-}

incr, triplo :: Integer -> Integer
incr x = x+1
triplo x = 3*x
boasVindas :: String -> String
boasVindas nome = "Olá, " ++ nome ++ "!"

{-
    1.2 Para que três valores possam ser medidas dos lados de um triângulo 
deve verificar-se a seguinte condição: qualquer dos valores deve ser 
inferior à soma dos outros dois. Complete a definição de uma função que 
testa esta condição; o resultado deve ser um valor boleano (True ou False).

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = a > 0 && b > 0 && c > 0 && 
                       a + b > c && a + c > b && b + c > a
-}

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = a > 0 && b > 0 && c > 0 && 
                       a + b > c && a + c > b && b + c > a

{-
    1.3 Podemos calcular a área A de um triângulo de lados a, b, c usando a
fórmula de Heron:

A = √s(s − a)(s − b)(s − c),

onde s = (a + b + c)/2. Complete a seguinte definição em Haskell duma 
função para calcular esta área.

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
                      where s = (a + b + c) / 2
-}

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
                      where s = (a + b + c) / 2

{-
    1.4 Usando as funções length, take, drop apresentadas na primeira aula,
escreva uma função metades que divide uma lista em duas com metade do
comprimento (aproximadamente). Exemplo:

metades [1,2,3,4,5,6,7,8] == ([1,2,3,4], [5,6,7,8])

Experimente a sua definição no interpretador e investigue o acontece se a 
lista tiver comprimento ímpar

metades :: [a] -> ([a], [a])
metades a = ((take size a), drop size a)
            where size = div (length a) 2

Em caso de uma lista com comprimento impar a segunda metade fica com um
elemento a mais. Por quê
-}

metades :: [a] -> ([a], [a])
metades a = ((take size a), drop size a)
            where size = div (length a) 2

{-
    1.5 Neste exercício pretende-se que use as funções o prelúdio-padrão de 
processamento de lista apresentadas na primeira aula: head, tail, length, 
take, drop e reverse.

(a) Mostre que a função last (que obtém o último elemento de uma lista) 
pode ser escrita como composição de algumas das funções acima. Consegue
encontrar duas definições diferentes?

testLast :: [a] -> a
testLast a = head (reverse a)

testLast2 :: [a] -> a
testLast2 a = head (drop size a)
              where size = (length a) - 1

(b) Analogamente, mostre que a função init (que remove o último elemento
duma lista) pode ser definida usando as funções acima de duas formas
diferentes.

testInit :: [a] -> [a]
testInit a = reverse (tail (reverse a))

testInit2 :: [a] -> [a]
testInit2 a = take size a
              where size = (length a) - 1
-}

testLast :: [a] -> a
testLast a = head (reverse a)

testLast2 :: [a] -> a
testLast2 a = head (drop size a)
              where size = (length a) - 1

testInit :: [a] -> [a]
testInit a = reverse (tail (reverse a))

testInit2 :: [a] -> [a]
testInit2 a = take size a
              where size = (length a) - 1

{-
    1.6 Os coeficientes binomiais (n k) são os números que aparecem como 
coeficientes dos termos Xk na expansão de (1 + X)^n; correspondem também 
ao número de formas distintas de escolher k objetos entre n (ou, 
equivalentemente, o número de subconjuntos com k elementos que podemos 
formar de um conjunto de n elementos). Neste exercício pretende-se calcular 
estes coeficientes para quaisquer n e k.

(a) Complete a definição duma função

para calcular o coefcientes binomial de n e k pela seguinte fórmula:

(n k) = n! / k!(n − k)!

Sugestão: pode exprimir n! como product [1..n].

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` 
            (product [product [1..k], product [1..(n-k)]])

(b) Podemos escrever a fórmula acima para calcular o mesmo resultado mas
evitando multiplicações desnecessárias. Por exemplo, podemos calcular 
(10 2) sem ter de calcular 10! e 8!:

(10 2) = 10! / (2! × 8!) = (10 × 9 × 8!) / (2 × 1 × 8!) = (10 × 9) / 2 = 45

Usando esta simplificação escreva uma definição alternativa binom' com
o mesmo tipo e que produz os mesmos resultados mas efetuando menos
cálculos.

Sugestão: Se k < n − k, o numerador reduz-se a ao produto dos números
de n − k + 1 até n e o denominador a k!. No caso em que k ≥ n − k, o
numerador reduz-se ao produto dois números de k+1 até n e o denominador
a (n − k)!.

binom' :: Integer -> Integer -> Integer
binom' n k = if k < n - k then case1 else case2
             where
                case1 = product [n-k+1..n] `div` product [1..k]
                case2 = product [k+1..n] `div` product [1..n-k]
-}

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` 
            (product [product [1..k], product [1..(n-k)]])

binom' :: Integer -> Integer -> Integer
binom' n k = if k < n - k then case1 else case2
             where
                case1 = product [n-k+1..n] `div` product [1..k]
                case2 = product [k+1..n] `div` product [1..n-k]