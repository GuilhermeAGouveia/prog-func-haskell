{- Correção 
--Questões compostas: 1(a-f), 2(a-e), 8 (a-e), 14 (a-c), 15 (a-e), 25 (a-c), 28 (a-c)--
---------------------------------------------------
Questões em BRANCO: total 0
Questões erradas: 0

NOTA: (53-0)*100/53 = 100

---------------------------------------------------}


import Data.Char
{------------------------------------------ 
Prova 02 Programação Funcinonal 18/08/2021
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

{----Questão 1 ------------------------------------------------------}

--Questão 1.a

type Brinde = (String, Int, Int)

tamanhoMapa :: Int
tamanhoMapa = 7

mapa :: Int -> Brinde

mapa 1 = ("Natal", 21, 34)
mapa 2 = ("Bertioga", 17, 65)
mapa 3 = ("Rio de Janeiro", 9, 10)
mapa 4 = ("Curitiba", 3, 54)
mapa 5 = ("Petrolina", 2, 09)
mapa 6 = ("Salvador", 0, 01)
mapa 7 = ("Teresina", 21, 56)
mapa x = ("Error", -1, -1)

--Questão 1.b

cidade :: Brinde -> String
cidade (a,b,c) = a 

nPassagens :: Brinde -> Int
nPassagens (a,b,c) = b

nHospedagens :: Brinde -> Int
nHospedagens (a,b,c) = c 

--Questão 1.c

funcQ1c :: Int -> Int
funcQ1c 0 = 0
funcQ1c x = nPassagens  (mapa x) + funcQ1c (x-1)

--Questão 1.d

funcQ1d :: Int -> Int
funcQ1d 0 = 0
funcQ1d x = nHospedagens  (mapa x) + funcQ1d (x-1)

--Questão 1.e

funcQ1eMain :: String -> Int -> (Int, Int, Int)
funcQ1eMain _ 0 = (0, 0, 0)
funcQ1eMain c x
    | c == cidade (mapa x) = (x, nPassagens (mapa x), nHospedagens (mapa x))
    | otherwise = funcQ1eMain c (x-1)

funcQ1e :: String -> (Int, Int, Int)
funcQ1e c = funcQ1eMain c tamanhoMapa

--Questão 1.f

--retorna o primeiro da tupla
fstT :: (u, v, w) -> u
fstT (a, b, c) = a

--retorna o segundo da tupla
sndT :: (u, v, w) -> v
sndT (a, b, c) = b

--retorna o terceiro da tupla
trdT :: (u, v, w) -> w
trdT (a, b, c) = c

funcQ1f :: Brinde -> Bool
funcQ1f (a,b,c) = fstT terna /= 0 && b <= sndT terna && c <= trdT terna
    where 
        terna = funcQ1e a 


{----Questão 2 ------------------------------------------------------}

--Questão 2.a

{-
Dado uma lista de duplas (a, b) a função retorna a dupla que possuir o menor b (que é um valor inteiro).
Exemplo: 
Feito a chamada: proximo [('a', 3), ('c', 8), ('x', 2), ('y', 4)]
O retorno é: ('x', 2), que é a dupla de maior prioridade ou a que possui o menor b
-}

--Questão 2.b

{-
Dado uma dupla (k, m) e uma lista de duplas (a,b), a função remove a dupla que possuir b igual a m e retorna o restante da lista 
Exemplo:
Feito a chamada: realizado ('x', 2) [('a', 3), ('c', 8), ('x', 2), ('y', 1)]
O retorno é: [('a',3),('c',8),('y',1)], com ('x', 2) sendo removido da lista
-}

--Questão 2.c

{-
Dado uma lista x que contém duplas (a,b), de acordo com a prioridade estabelecida em b (que é tratada pela funcao proximo), a funcao f01 
pega a e o joga em uma lista de caracteres que é montada recursivamente, sempre que a é inserido nesta lista de caracteres a dupla que o contém
é removida da lista x (ação feita pela funcao realizado) e a função f01 é chamada recursivamente a partir desta nova lista x, agora sem (a,b).
Exemplo:
Feito a chamada: f01 [('t', 3), ('u', 8), ('a', 2), ('t', 1)]
O retorno é: "tatu"
-}

--Questão 2.d

{-
a funcao proximo é apenas uma parte da solução que é usada em f01, ou seja, ela só é instanciada por f01 e não foi feita para uso direto, 
como f01 já faz o tratamento para listas vazias recebidas em x, não é necessário que proximo trate.
-}

--Questão 2.e

{-
t: (Char, Int)
a: Char
b: Int
c: Char
x: [(Char. Int)]
-}

{----Questão 3 ------------------------------------------------------}

bias :: Int
bias = ord 'a'

convertString :: [Int] -> String
convertString l = [chr (c + bias) | c<-l]

--verifica se algum caracter de l está em l1
searchString :: String -> String -> Bool
searchString l l1 = length [a | a <- l, a `elem` l1] > 0

funcQ3 :: [[Int]] -> String -> [String]
funcQ3 l s = [convertString a | a<-l, searchString s (convertString a)]



{----Questão 4 ------------------------------------------------------}


funcQ4 :: [String] -> [(Int,String)]
funcQ4 l = [(length a, a) | a<-l]


{----Questão 5 ------------------------------------------------------}

funcQ5 :: [([Int], Bool)] -> [[Int]]
funcQ5 x =  map trataLista x

trataLista :: ([Int], Bool) -> [Int]
trataLista (a,b)
    | b = a
    | otherwise = retiraImpar True a

--se o parametro b tiver como valor inicial True, os impares são removidos
--se o parametro b tiver como valor inicial False, os pares são removidos
retiraImpar :: Bool -> [Int] -> [Int]
retiraImpar _ [] = []
retiraImpar b (a:x)
    | b = a : retiraImpar False x
    | otherwise = retiraImpar True x



{----Questão 6 ------------------------------------------------------}

funcQ6 :: [Int] -> [Int]
funcQ6 [] = []
funcQ6 [u] = [u]
funcQ6 (a:b:l)
    | mod b 2 == 0 && mod a 2 /= 0 = a : funcQ6 l
    | otherwise = a : funcQ6 (b:l)

{----Questão 7 ------------------------------------------------------}

funcQ70 :: [Int] -> [Int] -> Int -> [Int]

funcQ70 [_] [] cont = [cont]
funcQ70 _ [] _ = []

funcQ70 (a:l1) (b:l2) cont
    |a == b = funcQ70 l1 l2 (cont + 1)
    |otherwise = cont : funcQ70 l1 (b:l2) (cont + 1)


funcQ7 :: [Int] -> [Int] -> ([Int], [Int])
funcQ7 l1 l2 = (l1, funcQ70 l1 l2 0)

{----Questão 8 ------------------------------------------------------}

type Subordinadas = [(String, Bool, [Int])]
lSubordinadas :: Subordinadas
lSubordinadas = [
                    ("1", False, [3000, 1200, 400, 2000, 40, 100, 600, 200, 120, 222, 1101, 200]),
                    ("2", True, [3000, 100, 00, 0, 0, 00, 00, 0, 12120, 12222, 10101, 900902]),
                    ("3", True, [3000, 1200, 4000, 2000, 400, 500, 600, 200, 12120, 2222, 10101, 90902])
               ]

meta :: Int
meta = 120
--Questão 8.a
funcQ8a1 :: Subordinadas -> [String]
funcQ8a1 l = [a | (a, b, c)<-l, b]

funcQ8a2 :: Subordinadas -> [String]
funcQ8a2 l = [a | (a, b, c)<-l, not b]

--Questão 8.b
getListaSemanal :: String -> Subordinadas -> [Int]
getListaSemanal _ [] = []
getListaSemanal id ((a, b, c):x)
    | id == a = c
    | otherwise = getListaSemanal id x

atingiuMetaMain :: [Int] -> Int -> Bool
atingiuMetaMain _ 6 = True
atingiuMetaMain [] _ = False
atingiuMetaMain (a:l) cont
    | a >= meta = atingiuMetaMain l (cont + 1)
    | otherwise = atingiuMetaMain l 0

funcQ8b :: String -> Bool
funcQ8b id = atingiuMetaMain (getListaSemanal id lSubordinadas) 0

--Questão 8.c

atingiuMetaAllMain :: Subordinadas -> [(String, Bool)]
atingiuMetaAllMain l = [(a, funcQ8b a) | (a, b, c) <- l] 

funcQ8c :: [(String, Bool)]
funcQ8c = atingiuMetaAllMain lSubordinadas

--Questão 8.d
funcQ8d :: [String]
funcQ8d = [a | (a,b)<-funcQ8c, b, elem a (funcQ8a2 lSubordinadas)]

--Questão 8.e

{-
Poderia definir uma dupla do tipo ([(String, [Int])], [(String, [Int])]), onde o primeiro elemento (uma lista) contém as filias
e o segundo elemento (outra lista) contém as candidatas.

Mas ainda assim vai haver necessidade de algumas modificações no código
-}

{----Questão 9 ------------------------------------------------------}



funcQ9 :: String -> Int
funcQ9 s = length [a | a<-s, a >= '0' && a <= '9']

{----Questão 10 ------------------------------------------------------}

getPos :: [Int] -> Int -> Int
getPos [] _ = -1
getPos (a:l) x 
    | x == 0 = a
    | otherwise = getPos l (x-1)

funcQ10 :: [Int] -> Int -> Bool

funcQ10 l x = x == getPos l x 

{----Questão 11 ------------------------------------------------------}

funcQ11 :: [(Int,Int)]->Int->[Bool]
funcQ11 l x = [(fst a + snd a) > x | a<-l]

{----Questão 12 ------------------------------------------------------}
funcQ12 :: Int -> Int -> ([Int], [Int])
funcQ12 x y = ([a | a<-[x..y], mod a x == 0], [a | a<-[x..y], mod a x /= 0])



{----Questão 13 ------------------------------------------------------}


{-Essa é uma sacada usando apenas list complehension, a<-x funciona como um contador, 
que ocorrera length x vezes, b são os caracteres sendo extraídos individualmente para formar a String
Seria simples fazer usando recursão, só que seria necessário duas funções uma repete a string n vezes,
e outra que chama repete passando a string e o tamanho da string-}
funcQ13 :: String -> String

funcQ13 x = [b | a<-x, b<-x]

{----Questão 14 ------------------------------------------------------}

--a) funcoes que recebem uma ou mais funções como argumentos ou
-- que devolvem outra função como valor de retorno
-- Funções de alta ordem tornam a base de código declarativa. 

--b)Além de permitir maior flexibilidade e menos repetição de código, quando leio um código escrito por outro desenvolvedor estas funções 
--me apontam o caminho tomado na resolução de um problema. Eu argumentaria que tal código 
--é mais conciso e fácil de entender do que um código imperativo que recorra a instruções de laço 
--(for/while/etc) e estruturas condicionais (if/else).

--c)A avaliação preguiçosa tem a vantagem de ser capaz de criar listas infinitas calculáveis ​​sem loops infinitos ou 
--questões de tamanho interferindo na computação. Por exemplo, pode-se criar uma função que cria uma lista infinita 
--(geralmente chamada de fluxo) de números de Fibonacci. O cálculo do n-ésimo número de Fibonacci seria meramente a 
--extração daquele elemento da lista infinita, forçando a avaliação apenas dos primeiros n membros da lista.
--fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{----Questão 15 ------------------------------------------------------}

type Id = Int
type Nome = String
type Telefone = String

type Pessoa = [(Id, Nome, Telefone)]
type Conhece = [(Id, [Id])]

lPessoa :: Pessoa
lPessoa = [(1,"Guilherme", "997322378"), (2,"Joao", "997322372"), (3,"Dudu", "997322371"), (4,"Elaine", "997322370")]

lConhece :: Conhece
lConhece = [(1, [2,3,4]), (2, [3]), (3, []), (4, [2,3])]

getIdForName :: Nome ->  Pessoa -> Id
getIdForName _ [] = -1
getIdForName s ((a, b, c):x)
    |s == b = a
    |otherwise = getIdForName s x

getNameForId :: Id ->  Pessoa -> Nome
getNameForId _ [] = ""
getNameForId s ((a, b, c):x)
    |s == a = b
    |otherwise = getNameForId s x

getConhecidos :: Nome -> Conhece -> [Id]
getConhecidos _ [] = []
getConhecidos n (a:x)
    | id == fst a = snd a
    | otherwise = getConhecidos n x
    where 
        id = getIdForName n lPessoa

validaNome :: Nome -> Pessoa -> Bool
validaNome _ [] = False
validaNome n ((a,b,c):l) = n == b || validaNome n l

--Questão 15.a

funcQ15a :: Nome -> Int

funcQ15a n 
    | validaNome n lPessoa = length (getConhecidos n lConhece)
    | otherwise = -1

--Questão 15.b

funcQ15b :: Nome -> [Nome]

funcQ15b n
    | validaNome n lPessoa = [getNameForId y lPessoa | y<-(getConhecidos n lConhece)]
    | otherwise = ["Nenhuma pessoa com o nome '" ++ n ++ "' foi encontrada"]

--Valida nome é usada para que seja possivel determinar se o usuário existe ou não, note n conhecidos tem que retornar uma lista,
--se a pessoa não existe na lista retorna [], se ela existe e não tem conhecidos tbm retorna [], logo usando apenas ela não da pra 
--dizer se uma pessoa realmente exste na lista ou não

--Questão 15.c

funcQ15cMain :: Nome -> Pessoa -> Telefone

funcQ15cMain n ((_,b,c):x)
    | b == n = c
    | otherwise = funcQ15cMain n x

funcQ15c :: Nome -> Telefone
funcQ15c n = funcQ15cMain n lPessoa

--Questão 15.d
{-
Para cada dupla x na lista lPessoa, se o id da pessoa de nome n não for elemento de sndT x (segundo elemento da tupla), quer dizer que n não conhece b
-}
funcQ15d :: Nome -> [Nome]

funcQ15d n 
    | validaNome n lPessoa = [ b | (a,b,c)<-lPessoa, not (a `elem` conhecidos), b /= n]
    | otherwise = ["Nenhuma pessoa com o nome '" ++ n ++ "' foi encontrada"]
    
    where 
        conhecidos = getConhecidos n lConhece


--Questão 15.e

--Em matemática discreta vimos uma operação de conjuntos chama intersecção de familia de conjuntos, que dado uma familia F de conjuntos A (ou conjunto de conjuntos), e retornado todos os elementos que pertencem a intersecção entre todos os conjuntos A da familia F,
--VOU DEFINIR ESSE OPERADOR EM HASKELL PARA RESOLVER O PROBLEMA PROPOSTO

-- funcao de intersecção

inter l1 l2 = removeDuplicados [x | x<-l1, y<-l2, x == y]

-- funcao de intersecção de familia de conjuntos

interF [u] = u
interF (a:ll) = inter a (interF ll)

funcQ15e :: [Nome]
funcQ15e = interF [(funcQ15d x) ++ [x] | (a,x,c)<-lPessoa]

{----Questão 16 ------------------------------------------------------}

findPos :: String -> Int -> Char
findPos [] _ = '*'
findPos (a:l) x 
    | x == 0 = a
    | otherwise = findPos l (x-1)

funcQ16 :: [(Int, String)] -> String
funcQ16 [] = []
funcQ16 (a:x) = findPos (snd a) (fst a) : funcQ16 x

{----Questão 17 ------------------------------------------------------}

nats = 1 : map (+1) nats

{----Questão 18 ------------------------------------------------------}

filtraElimina :: (Int->Bool) -> [Int]->[Int]

filtraElimina f l = [x | x<-l, not (f x)]

isPar :: Int -> Bool
isPar x = mod x 2 == 0

{----Questão 19 ------------------------------------------------------}

funcQ19 :: [Int] -> ([Int], [Int], [Int])

funcQ19 l = ([a | a<-l, mod a 2 == 0], [b | b <-l, mod b 3 == 0], [c | c<-l, (mod c 3 /= 0) && (mod c 2 /= 0)])

{----Questão 20 ------------------------------------------------------}

{-
maxList [] = -999999 só ocorre se a função tiver como paremetro inicial lista vazia, coloquei -999999 como código de erro, 
mas o fato é que não dá para expressar um erro como um inteiro para o contexto desta função, já que todo inteiro pode ser como resultado 
de um retorno com ẽxito, por exemplo, -999999 pode realmente ser o maximo de uma lista não vazia, gerando ambiguidade. 
-}
maxList :: [Int] -> Int
maxList [] = -999999
maxList [t] = t
maxList (a:b:x)
    |a < b = maxList (b:x)
    |otherwise = maxList (a:x)

funcQ20 :: [[Int]] -> [Int]

funcQ20 ll = [maxList l | l<-ll]

{----Questão 21 ------------------------------------------------------}

testa :: Char -> String -> [Bool]

testa c l = [c==x | x<-l]

{----Questão 22 ------------------------------------------------------}

geraListaDupla :: [Int] -> Int -> [(Int, Int)]

geraListaDupla l x = [(a,x)| a<-l, a > x]

{----Questão 23 ------------------------------------------------------}

{-
Basicamente, para cada elemento x de l eu verifico se o tamanho da lista gerada pelos elementos iguais a x em l é maior que 1, se for é pq o elemento se repete. 
Obviamente isso será aplicado para todos elementos em l, e se há elementos repetidos, a resposta também terá elementos repetidos, para contornar isso uso  a funcao removeDuplicados.
Com certeza não é a solução de maior desempenho, mas a maneira como é feito o uso de list complehension foi legal e genuína.
-}

removeDuplicados [] = []
removeDuplicados (a:x)
    | elem a x = removeDuplicados x
    | otherwise =  a : removeDuplicados x

repetidos :: [Int] -> [Int]

repetidos l = removeDuplicados [x | x<-l, (length [c | c<-l, c==x ]) > 1]

{----Questão 24 ------------------------------------------------------}

--A condição foi necessária para o caso em que n for 0, supondo que "entre 0 e 100" inclua 0 e 100.

termina_em :: Int -> [Int]

termina_em n = [x + n | x<-[0, 10 .. 100], x + n <= 100]

{----Questão 25 ------------------------------------------------------}

--Questão 25.a

infix 7 &&&
(&&&) :: Int -> (Int, Int) -> Int

(&&&) a (b,c)
    | abs (a - b) < abs (a - c) = b
    | otherwise = c

--Questão 25.b

funcQ25b :: (Int -> (Int, Int) -> Int) -> Int -> [(Int, Int)] -> [Int]

--Usando list comprehension
funcQ25b f x listaDuplas = [f x a | a <-listaDuplas]

--Usando map
funcQ25b_Map f x listaDuplas = map (f x) listaDuplas

--Questão 25.c

{-
Chamada: funcQ25b (&&&) 5 [(4,2),(5,3), (20, 1), (8,2)]

Para cada elemento a em listaDuplas, é feito o uso de a na função f cujo seu primeiro parametro é x (fixo), 
e seu segundo parametro é a (cada elemento da listaDuplas), o valor inteiro retornado por f será usando na 
formação da lista resultante da estrutura list complehension

Abstraindo o pensando, o que acontece é o seguinte:

Para: funcQ25b (&&&) 5 [(4,2),(5,3), (20, 1), (8,2)]

É retornado:

[(&&&) 5 (4,2), (&&&) 5 (5,3), (&&&) 5 (20,1), (&&&) 5 (8,2)], que é o mesmo que: [4,5,1,2]
-}

{----Questão 26 ------------------------------------------------------}

--A função removeDuplicados já foi definida no exercicio 23

--Usando list comprehension
mescla :: [Char] -> [Char] -> [(Char, Int)]
mescla a b = [ (x, length [c | c<-b, c==x] ) |x<-removeDuplicados a]

--Usando recursão

mescla2 :: [Char] -> [Char] -> [(Char, Int)]
mescla2 [] _ = []
mescla2 (a:x) l 
    | elem a x =  mescla2 x l
    | otherwise = (a, length [c | c<-l, c==a] ) :  mescla2 x l

{----Questão 27 ------------------------------------------------------}

infix 3 -*-

(-*-) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(-*-) a b = (maior (maior (fst a) (snd a)) (maior (fst b) (snd b)) , menor (menor (fst a) (snd a)) (menor (fst b) (snd b)))

maior :: Int -> Int -> Int
maior a b
    | a > b = a
    | otherwise = b

menor :: Int -> Int -> Int
menor a b
    | a < b = a
    | otherwise = b

{----Questão 28 ------------------------------------------------------}

--Questão 28.a

ocorrencia :: Int -> [Int] -> (Int, Int)
ocorrencia x l = (x, length [ a | a<-l, a==x])

--Questão 28.b

aplica :: (Int -> [Int] -> (Int, Int)) -> [Int] -> [(Int, Int)]
aplica f k = [f a k | a<-k]

--Questão 28.c

{-
a maneira de se utilizar é a seguinte: aplica ocorrencia [1,2,3,4,2,1,2,3,5,3]
o primeiro argumento é a função, já o segundo é a lista, como está na assinatura
de aplica, cada valor a desta lista será usado juntamente com k para chamar a função f.
-}

{----Questão 29 ------------------------------------------------------}

funny :: Int -> Int -> Int -> Bool

funny x y z = x > z || not (y >= x)

{----Questão 30 ------------------------------------------------------}

{-
O deixei associativo a direita pois assim posso fazer 2 # 3 # [2,1,1,2,3,4,5,3], que é o mesmo que 2 # (3 # [2,1,1,2,3,4,5,3]), tornando o operador mais atrativo
-}
infixr 7 # 
(#) :: Int -> [Int] -> [Int]
(#) _ [] = []
(#) x (a:b)
    | x == a = b
    | otherwise = a : ((#) x b)
