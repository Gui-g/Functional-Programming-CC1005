{-
2.1 Escreva duas definições, respectivamente usando expressões condicionais e
guardas, da função classifica :: Int -> String que faz corresponder uma
classificação qualitativa a uma nota de 0 a 20:
≤ 9 reprovado
10-12 suficiente
13-15 bom
16-18 muito bom
19-20 muito bom com distinção
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use camelCase" #-}
import GHC.Float (powerFloat)
{-# HLINT ignore "Use guards" #-}

classifica1 :: Integer -> String
classifica1 n = 
    if n <= 9 then "reprovado"
    else if 10 <= n && n <= 12 then "suficiente"
    else if 13 <= n && n <= 15 then "bom"
    else if 16 <= n && n <= 18 then "muito bom"
    else if 19 <= n && n <= 20 then "muito bom com distinção" else "nothing"

classifica2 :: Integer -> String
classifica2 n
  | n <= 9 = "reprovado"
  | 10 <= n && n <= 12 = "suficiente"
  | 13 <= n && n <= 15 = "bom"
  | 16 <= n && n <= 18 = "muito bom"
  | 19 <= n && n <= 20 = "muito bom com distinção"
  | otherwise = "nothing"


{-
O índice de massa corporal (IMC) é uma medida simples para classificar
o peso de adultos. O IMC de um indivíduo é calculado como o valor do peso
(em quilogramas) a dividir pelo quadrado da altura (em metros):
IMC = peso/altura²
Por exemplo: um indíviduo com 70Kg e 1.70m de altura tem IMC igual a 70/1.70² ≈
24.22. Classificamos o resultado nos seguinte intervalos:
IMC < 18.5 "baixo peso"
18.5 ≤ IMC < 25 "peso normal"
25 ≤ IMC < 30 "excesso de peso"
30 ≤ IMC "obsesidade"
Escreva uma definição da função classifica :: Float -> Float -> String
que determina a classificação acima; os dois argumentos da função são, respec-
tivamente, o peso em quilogramas e a altura em metros.
-}

classificaIMC :: Float -> Float -> String
classificaIMC w h
  | w / powerFloat h 2 < 18.5 = "baixo peso"
  | 18.5 <= w / powerFloat h 2 && w / powerFloat h 2 < 25 = "peso normal"
  | 25 <= w / powerFloat h 2 && w / powerFloat h 2 < 30 = "excesso de peso"
  | 30 <= w / powerFloat h 2 = "obsesidade"
  | otherwise = "nothing"

{-
Considere duas possíveis definições das funções max e min do prelúdio-padrão que calculam, 
respectivamente, o máximo e o mínimo de dois valores:
max, min :: Ord a => a -> a -> a
max x y = if x>=y then x else y
min x y = if x<=y then x else y

(a) Escreva definições deste género para duas funções max3 e min3 para cal-
cular, respectivamente, o máximo e o mínimo de três números.
(b) Observe que as operação de máximo e mínimo são associativas. Por exem-
plo, para calcular o máximo de três valores podemos determinar o máximo
entre dois deles e depois o máximo do resultado com o terceiro. Re-escreva
as funções max3 min3 usando esta ideia e as funções de max e min do
prelúdio-padrão
-}

max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z = if x >= y && x >= z then x else if y >= x && y >= z then y else z
min3 x y z = if x <= y && x <= z then x else if y <= x && y <= z then y else z

max3', min3' :: Ord a => a -> a -> a -> a
max3' x y z = max (max x y) z
min3' x y z = min (min x y) z

{-
Escreva uma definição da função lógica ou-exclusivo
xor :: Bool -> Bool -> Bool
usando múltiplas equações com padrões.
-}

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

{-
Pretende-se implementar uma função safetail :: [a] -> [a] que 
extende a função tail do prelúdio de forma a dar a lista vazia quando o argumento
é a lista vazia (em vez de um erro). Escreva três definições diferentes usando
condicionais, equações com guardas e padrões.
-}

safetail :: [a] -> [a]
safetail a = if null a then [] else tail a

safetail2 :: [a] -> [a]
safetail2 a
    | null a = []
    | otherwise = tail a

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 a = tail a

{-
Escreva duas definições da função curta :: [a] -> Bool para testar se
uma lista tem zero, um ou dois elementos, usando:
(a) a função length do prelúdio-padrão;
(b) múltiplas equações e padrões.
-}

curta :: [a] -> Bool
curta a = length a <= 3

curta2 :: [a] -> Bool
curta2 [] = True
curta2 [x] = True
curta2 [x,y] = True
curta2 [x,y,z] = True
curta2 (x:y:z:rest) = False

{-
A mediana de três valores é o valor "no meio" quando os colocamos por
ordem crescente. Por exemplo: mediana 2 3 (-1) == 2.
(a) Escreva uma definição da função mediana para determinar a mediana de
3 valores quaisquer. Qual será o seu tipo mais geral? Note que podemos
determinar a mediana usando apenas comparações de ordem.
(b) Em vez de definir a mediana diretamente usando comparações (que pro-
vavelmente terá sido a sua primeira ideia), pode usar o seguinte método:
somamos os 3 valores e subtraimos o maior e o menor. Re-defina a função
mediana desta forma. Qual será agora o tipo mais geral?
-}

mediana :: Ord a => a -> a -> a -> a
mediana x y z = if (x >= y && y >= z) || (z >= y && y >= x) then y else if (z >= x && x >= y) || (y >= x && x >= z) then x else z

mediana2 :: Integer -> Integer -> Integer -> Integer
mediana2 x y z = (x + y + z) - max3 x y z - min3 x y z

{-
2.8 ~ solução professor

Defina uma função
converte :: Int -> String
para converter um inteiro positivo inferior a 1 milhão para texto em português.
Alguns exemplos:
converte 21 = "vinte e um"
converte 1234 = "mil duzentos e trinta e quatro"
converte 123456
= "cento e vinte e três mil quatrocentos e cinquenta e seis"
Ideia: Vamos começar por definir funções auxiliares para converter para texto
os números inferiores a 100 e 1000. Este exercício será resolvido numa aula
teórica.
-}

unidades :: [String]
unidades =
  [ "zero"
  , "um"
  , "dois"
  , "tres"
  , "quatro"
  , "cinco"
  , "seis"
  , "sete"
  , "oito"
  , "nove"
  ]

dez_a_dezanove :: [String]
dez_a_dezanove =
  [ "dez"
  , "onze"
  , "doze"
  , "treze"
  , "quatorze"
  , "quinze"
  , "dezasseis"
  , "dezassete"
  , "dezoito"
  , "dezanove"
  ]

dezenas :: [String]
dezenas =
  [ "vinte"
  , "trinta"
  , "quarenta"
  , "cinquenta"
  , "sessenta"
  , "setenta"
  , "oitenta"
  , "noventa"
  ]

{-
  A função 'converte2' é composição de duas:
  * 'divide2' obtêm os algarimos;
  * 'combina2' combina o texto de cada algarismo.
  Usamos as operações de concatenação (++) e
  indexação de listas (!!) (note que os índices começam em zero.)
-}
converte2 :: Int -> String
converte2 n | n<100 = combina2 (divide2 n)

divide2 :: Int -> (Int, Int)
divide2 n = (n`div`10, n`mod`10) -- (quociente,resto)

combina2 :: (Int, Int) -> String
combina2 (0, u) = unidades !! u
combina2 (1, u) = dez_a_dezanove !! u
combina2 (d, 0) = dezenas !! (d-2)
combina2 (d, u) = dezenas !! (d-2) ++ " e " ++ unidades !! u

{- Em seguida, resolvemos o problema análogo para números até 3
   algarismos. Necessitamos dos nomes em Português das centenas.
 -}
centenas :: [String]
centenas =
  [ "cento"
  , "duzentos"
  , "trezentos"
  , "quatrocentos"
  , "quinhentos"
  , "seiscentos"
  , "setecentos"
  , "oitocentos"
  , "novecentos"
  ]

{- A função de conversão, nos mesmos moldes da anterior.
   Note o tratamento especial do número 100.  -}
converte3 :: Int -> String
converte3 n | n<1000 = combina3 (divide3 n)

divide3 :: Int -> (Int, Int)
divide3 n = (n`div`100, n`mod`100)

combina3 :: (Int, Int) -> String
combina3 (0, n) = converte2 n
combina3 (1, 0) = "cem"
combina3 (c, 0) = centenas !! (c-1)
combina3 (c, n) = centenas !! (c-1) ++ " e " ++ converte2 n

{- Finalmente podemos resolver o problema para números
  até 6 algarismos, i.e. inferiores a 1 milhão.  -}
converte :: Int -> String
converte n | n<1000000 = combina (divide n)

divide n = (n `div` 1000, n `mod` 1000)

combina (0, n) = converte3 n
combina (1, 0) = "mil"
combina (1, n) = "mil" ++ ligar n ++ converte3 n
combina (m, 0) = converte3 m ++ " mil"
combina (m, n) = converte3 m ++ " mil" ++ ligar n ++ converte3 n

{- Uma função auxiliar para escolher a partícula de ligação entre
   milhares e o restante (r).
   Regra: colocamos "e" quando o resto é inferior a 100
   ou múltiplo de 100; caso contrario, basta um espaço.
 -}
ligar :: Int -> String
ligar r
  | r < 100 || r `mod` 100 == 0 = " e "
  | otherwise                   = " "