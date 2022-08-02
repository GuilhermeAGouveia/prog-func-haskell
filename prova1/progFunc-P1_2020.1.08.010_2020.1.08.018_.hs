{---------------------- Correção -------------- 

Compilação: correta
Em branco = 0
Erros = 0

Nota: ((40-0)/40)*100= 100
----------------------------------------------}


{------------------------------------------ 
Prova 01 Programação Funcinonal 07/07/2021
-------------------------------------------
Nome completo 01: Guilherme Augusto Gouveia
Matricula 01: 2020.1.08.010

Nome completo 02: Victor Hugo Tozzo Filho
Matrícula 02: 2020.1.08.018

-------------------------------------------
Instruções para o preenchimento do script
a) Implemente o código na sequência em que aparece na prova
b) separe cada questão com as linhas pontilhadas (abaixo)
   {--Questão X -----------------------------------------------}
c) Questões com mais de um item, separe-os da seguinte forma:

{--Questão X ----------------------------------------------}

--Item X.a
 código da questão X item a

--Item X.b
  código da questão X item b

IMPORTANTE: Todo o script deve ter as citações de todas as questões e itens.

Então, as questões ou itens não solucionados por você deverão receber, impreterivelmete, o texto --EM BRANCO --
Caso você não organize a prova como indicado, poderá ser penalizado na nota final
  
d) Não use nomes de funções diferntes dos indicados nas qeustões.
De forma alternativa, e caso o nome não seja sugerido, dê preferência por começar como: funcQx, em que x refere-se à questão.
Ex: funcQ3a pode ser a implementação da questão 3, item a

e) não é necessário (mas não é um problema) incluir o enunciado das questões no script
f) Você pode inserir comentários pessoais explicando o código implementado antes do cabeçalho da função

g) QUESTÕES COM TÉCNICAS NÃO APRESENTADAS EM AULA NÃO SERÃO CONSIDERADAS


-------------------------------------------}

-- +++++++++++++++++ comece, aqui, sua prova. Boa prova +++++++++++++++++++++++ --
import Char

{----Questão 1 ------------------------------------------------------}

--Item 1.a 
--OBS: numneros negativos devem ser colocados entre ().

funcQ1a :: Int -> Int
funcQ1a x
 |x == (-4) || x == (-2) = 0
 |x >= 0 = (x+4)`div`(x+2)
 |x < 0 = 2`div`x

--Item 1.b

funcQ1b :: Int -> Int -> Int
funcQ1b x y
 |x >= y = x+y
 |x < y = x-y

--Item 1.c

funcQ1c :: Int -> Int -> Int -> Int
funcQ1c x y z
 |x+y > z = x+y+z
 |x+y < z = x-(y-z)
 |x+y == z = 0

{----Questão 2 ------------------------------------------------------}

{- a funcao esta errada pois falta determinar o parametro de parada (ou caso base),
 ou seja o fat zero -}

fat::Int->Int
fat 0 = 1
fat x = x * fat(x-1)

{----Questão 3 ------------------------------------------------------}

{- o exercicio pede que apenas use a funcao soma, nao sendo ela 
exclusiva para desenvolver a multiplicação, apenas auxilie -}

soma :: Int -> Int -> Int
soma x y = x+y

multip :: Int -> Int -> Int
multip x y
 |x == 1 = y
 |y == 1 = x
 |otherwise = soma x (x*(y-1))

{----Questão 4 ------------------------------------------------------}

invertIntMain :: String -> String

invertIntMain [] = []
invertIntMain (a:x) = invertIntMain x ++ [a]

invertInt :: Int -> Int
invertInt a = read (invertIntMain (show (a)))

{----Questão 5 ------------------------------------------------------}

fourPower :: Int -> Int
square x y = x^y
fourPower x = square x 4

{----Questão 6 ------------------------------------------------------}

inesimo :: Int -> Float
inesimo 0 = sqrt 6
inesimo x = sqrt(6 + inesimo(x-1))

{----Questão 7 ------------------------------------------------------}
{-Supondo que a ordem dos objetos não importa-}

funcQ7 :: Int -> Int -> Int
funcQ7 m n
    | m >= n = fat m `div` (fat n * fat (m - n))
    | otherwise = -1

{----Questão 8 ------------------------------------------------------}

mdc :: Int -> Int -> Int

mdc m n 
    | mod m n /= 0 = mdc n (mod m n)
    | otherwise = n

{----Questão 9 ------------------------------------------------------}

howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples v i f
  | i > f = 0
  | mod i v == 0 = 1 + howManyMultiples v (i+1) f
  | otherwise = howManyMultiples v (i+1) f


{----Questão 10 ------------------------------------------------------}

lastDigit :: Int -> Int
lastDigit num
    | (num < 0) = ((-1) * num) `rem` 10
    | otherwise = num `rem` 10

{----Questão 11 ------------------------------------------------------}

{- Uma solução comum para esse tipo de exercicio é a converção do numero em uma 
lista de caracteres, tornando assim facil capturar um algarismo específico em determinada posição, mas
eu decidir ir além e pensar mais, buscar uma solução que seja mais próxima da matemática,
algo que eu ainda não tinha visto alguém conseguir, usando operações de resto e divisão, 
tendo em vista que estou trabalhando com numeros na base 10, eu criei a solução abaixo: -}

lengthNumber :: Int -> Int
lengthNumber 0 = 0
lengthNumber n = 1 + lengthNumber (n `div` 10)

anyDigit :: Int -> Int -> Int
anyDigit x y
    -- |x > a = -1
    | x >= a = - 1 -- verifica se a posição é válida
    | otherwise = (mod y (10^b) - mod y (10^(b-1))) `div` (10^(b-1)) -- pulo do gato usando operações de resto e a divisão de inteiros
    where 
        a = lengthNumber y
        b = lengthNumber y - x 
        --b = x
        {-
        A funcao que criei foi pensada em calcular a posição da direita para esquerda começando da posição 1,  
        b é uma adaptação para que passe a calular posições da esquerda para direita começando da posição 0, 
        para entender a diferença, comente as linhas 151 e 155 e descomente as linhas 150 e 156.
        -}
       

{----Questão 12 ------------------------------------------------------}

--Item 12.a ele esquece da possibilidade de que m possa ser igual p ex: 1 2 1 

--Item 12.b apenas adicionar mais uma condição e evitar m=p

allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)


{----Questão 13 ------------------------------------------------------}

{-- xor é um (ou exclusiovo) para facilitar na função --}

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

howManyEquals :: Int -> Int -> Int -> Int
howManyEquals x y z
 |x == y && y == z = 3
 |xor(xor(x == y) (x == z)) (y == z) = 2
 |otherwise = 0

{----Questão 14 ------------------------------------------------------}

periodo :: Int
periodo = 7

vendas :: Int -> Int

vendas 0 = 10
vendas 1 = 20
vendas 2 = 9
vendas 3 = 30
vendas 4 = 2
vendas 5 = 10
vendas 6 = 2
vendas 7 = 0

--Item 14.a

howManyLess :: Int -> Int -> Int -> Int

howManyLess v i f 
 | i > f || i > periodo = 0
 | v > vendas i = 1 + howManyLess v (i + 1) f
 | otherwise = howManyLess v (i + 1) f

--Item 14.b

noZeroInPeriod :: Int -> Bool

noZeroInPeriod 0 = vendas 0 /= 0
noZeroInPeriod d = vendas d /= 0 && noZeroInPeriod (d - 1)

--Item 14.c

zerosInPeriodMain :: Int -> [Int]

zerosInPeriodMain (-1) = []
zerosInPeriodMain d
    | vendas d == 0 = d : zerosInPeriodMain (d-1)
    | otherwise = zerosInPeriodMain (d-1)

zerosInPeriod :: [Int] -- Para criar a funcao com a assinatura dada pelo enunciado, foi necessário o uso de uma interface, que oculta o parametro 'dia'.

zerosInPeriod = zerosInPeriodMain periodo

--Item 14.d

funcQ14d :: Int -> Int -> [Int]
funcQ14d _ (-1) = []
funcQ14d v d 
 | v > vendas d = d : funcQ14d v (d - 1)
 | otherwise = funcQ14d v (d - 1)


{----Questão 15 ------------------------------------------------------}

{-  antFibMain consiste em três parametros:

x: elemento anterior da sequencia
y: elemento atual da sequencia
z: o elemento que encontrar na sequencia

possui três condições em guardas:

guarda 1: se x é numero procurado. Retorno = 0
guarda 2: se x é menor que z, isso quer dizer que não chegamos até um z que pode estar na sequencia. 
          A recursão prossegue, agora y é o termo anterior e x+y é termo atual da sequencia, z apenas é repassado, a função carrega uma soma de +1.
guarda 3: ocorre se x maior que z, quer dizer que já passamos de z, e se não houve um x igual a z, z não pertence
          a sequencia. Retorno = numero negativo, para isso eu cancelo a soma da recursão até o primeiro numero maior que z, refazendo ela porem com um sinal de subtração, isso é aquivalente a x - x = 0 e faço -1.

antFib é apenas uma interface.
-}

antFibMain :: Int -> Int -> Int -> Int
antFibMain x y z
    | x == z = 0
    | x < z = 1 + antFibMain y (x+y) z
    | otherwise = - (1 + antFibMain 0 1 x)


antFib :: Int -> Int
antFib z = antFibMain 0 1 z

{----Questão 16 ------------------------------------------------------}

funny :: Int -> Int -> Int -> Bool
funny x y z = x > z || not(y >= x)

{----Questão 17 ------------------------------------------------------}

interval :: Int
interval = ord 'a' - ord 'A'
funcQ17 :: Char -> Char

funcQ17 x
 | 'a' <= x && x <= 'z' = chr (ord x - interval)
 | otherwise = x

{----Questão 18 ------------------------------------------------------}

bias :: Int
bias = ord '0'

charToNum :: Char -> Int
charToNum x 
 | '0' <= x && x <= '9' = ord x - bias
 | otherwise = -1

{----Questão 19 ------------------------------------------------------}

duplicate :: String -> Int -> String

duplicate s 0 = []
duplicate s x = s ++ duplicate s (x-1)

{----Questão 20 ------------------------------------------------------}

 {-Eu poderia fazer a funcao com 3 guardas, da seguinte maneira:
 | n < length s = s
 | n == length s = s
 | otherwise = ">" ++ pushRight s (n-1)
 
 mas isso não é inteligente
 
 -}

pushRight :: String -> Int -> String
pushRight s n
 | n <= length s = s
 | otherwise = ">" ++ pushRight s (n-1)

{----Questão 21 ------------------------------------------------------}
--infix 6 &-
(&-) :: Int -> Int -> Int
x &- y = x - 2*y

--Item 21.a
--Faz (10 - 2*3) - 2*2 que resulta em 0, ou seja, resolve da esquerda para direita.

--Item 21.b
--Faz 10 - 2*(3 - 2*2) que resulta em 12, ou seja, resolve da direita para esquerda.

--Item 21.c
--Gera ambiguidade, pois o compilador não sabe por onde começar em operações compostas
--cujo os operadores tem mesma precedência e não há uma associatividade (infixl ou infixr) definida.

--Item 21.d
{- 
Resulta em -2. Resolve a multiplicação primeiro, para depois com o resultado resolver
a operação definida por nosso operador, isso ocorre por conta da multiplicação
ter precedência maior que &- (7 > 6). Sendo assim, 10 &- 3 * 2 é equivalente a 10 &- (3 * 2)
-}

--Item 21.e 
{-
Resulta em 8. Resolve a operação &- primeiro, para depois com o resultado resolver
a multiplicação, isso ocorre por conta da multiplicação ter 
precedência menor que &- (7 < 8). Sendo assim, 10 &- 3 * 2 é equivalente a (10 &- 3) * 2
-}

{----Questão 22 ------------------------------------------------------}

inverte :: [Int] -> [Int]
inverte [] = []

inverte (a:x) = inverte(x) ++ [a]

{----Questão 23 ------------------------------------------------------}

--É válido lembrar que "" é equivalente a []

bias2 :: Int
bias2 = ord 'A' - 1


converte :: [Int] -> String
converte [] = ""
converte (a:x) = [chr (a + bias2)] ++ converte x

{----Questão 24 ------------------------------------------------------}

--Item 24.a

--R: "abcdefg"

--Item 24.b

--R: [0.1,1.1]
--Isso ocorre por que o hugs define por padrão a razão como sendo 1, 
--vendo isto como uma PA (Progressão Aritmética), a razão de progressão
--só altera se dois valores forem fornecido antes do operador .., 
--Exemplo se quizessemos de uma razão de 0.1, seria necessario escrever
--[0.1, 0.2 .. 0.9]

--Item 24.c
--R: [0.1,0.3,0.5,0.7,0.9]

--Item 24.d
--R: [0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9]


--Item 24.e
--R: [] 
-- A razão é -0.2, mas é pedido para que a lista vá de 0.4 até o primeiro numero menor ou igual a 0.8, como não é possivel retorna [].

--Item 24.f
--R: [1,4,7,10,13]

{----Questão 25 ------------------------------------------------------}

 {-
 O enunciado fala: "dado uma lista caracteres e um caracter a",
 mas na consulta de exemplo, ele usa o segundo parametro como sendo
 uma lista de caracteres com um unico elemento, pois "B" é equivalente a ['B'],
 e não a 'B', por isso defini o tipo do segundo parametro como sendo
 String, para ficar coerente ao exemplo.
 -}

conta :: String -> String -> Int

conta [] _ = 0
conta (a:x) y 
 | [a] == y = 1 + conta x y
 | otherwise = conta x y

{----Questão 26 ------------------------------------------------------}

purifica [] = [] --Tratamento de exceção, para o caso de ser passado lista vazia
purifica [x] = [x] --caso base para meu algoritmo.
purifica (a:b:x)
 | a == b = purifica (b:x)
 | otherwise = [a] ++ purifica (b:x)

{----Questão 27 ------------------------------------------------------}

{-  Essa funcao apenas monta uma lista com 'p' elementos, cujo todos elementos são 'a'
    Perceba que se p é um numero não positivo, é dado como retorno a lista vazia, pois
    não faz sentido repetir um elemento '0 vezes' ou 'negativas vezes' -}

mountList :: Int -> Int -> [Int]
mountList _ 0 = []
mountList a p
    | p <= 0 = []
    | otherwise = a : mountList a (p-1)
--

{-  Essa funcao retira cada 'a' da lista e chama a funcao mountList, o resultado é concateneado com repeatNumber x, 
    repeatNumber x é o ponto de recursão, então tupo se repete até que todos os elementos da lista sejam processados
    e x seja uma lista vazia -}
    
proliferaInt :: [Int] -> [Int]
proliferaInt [] = []


proliferaInt (a:x) = (mountList a a) ++ proliferaInt x

{----Questão 28 ------------------------------------------------------}

{-Adaptação da solução anterior para caracteres-}

bias3 :: Int
bias3 = ord 'A' - 1

mountListChar :: Char -> Int -> [Char]
mountListChar _ 0 = []
mountListChar a p
    | p <= 0 = []
    | otherwise = a : mountListChar a (p-1)

    
proliferaChar :: [Char] -> [Char]
proliferaChar [] = []
proliferaChar (a:x) = mountListChar a (ord a - bias3) ++ proliferaChar x
