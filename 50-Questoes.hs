module HelloRecurso where

import Data.List
import Data.Char 
import Data.Either
import Data.Maybe



-- Exercicio 1 (constroi a lista dos numeros inteiros compreendidos entre dois limites.)
enumFromTo':: Int->Int->[Int]
enumFromTo' x y = [x..y]
       ----------ou----------
enumFromTo'':: Int->Int->[Int]
enumFromTo'' x y | x==y = [x]
                 | x<y = x:enumFromTo'' (x+1) y
                 | otherwise = []



-- Exercicio 2 (constroi a lista dos numeros inteiros compreendidos entre dois limites e espacados de um valor constante.)
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x==z = [x]
                      | x<z = x:enumFromThenTo' (x+(y-1)) y z ---duvidas
                      | otherwise = []



-- Exercicio 3 (que concatena duas listas.) Ex:(++) [1,2,3] [10,20,30] corresponde a lista [1,2,3,10,20,30].
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h:t) l = h: ((+++) t l)



-- Exercicio 4 (calcula o elemento da lista que se encontra nessa posicao (assumese que o primeiro elemento se encontra na posicao 0).)
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h 
(!!!) (h:t) x = (!!!) t (x-1)
        --ou--  
(!?!) :: [a] -> Int -> a
(!?!) l x = head $ drop x l



--Exercicio 5 (dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = rev [] l
         where rev l [] = l
               rev l (h:t)= rev (h:l) t
           ----- ou ------
reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (h:t) = reverse t ++ [h]


-- Exercicio 6 (dado um inteiro n e uma lista l calcula a lista com os (no maximo) n primeiros elementos de l.)
take':: Int->[a]->[a]
take' 0 l = []
take' n (h:t) = h: take' (n-1) t
        -----ou-----
take'':: Int->[a]->[a]
take'' 0 l = []
take'' n [] = []
take'' n (h:t) | n>0 = h:take(n-1) t



-- Exercicio 7 (dado um inteiro n e uma lista l calcula a lista sem os (no maximo) n primeiros elementos de l. Se a lista fornecida tiver n elementos ou menos, a lista resultante sera vazia.)
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' n [] = []
drop' n (h:t) | n>0 = drop'(n-1) t



--Exercicio 8 (constroi uma lista de pares a partir de duas listas.)
zip':: [a] -> [b] -> [(a,b)]
zip' [] l2 = []
zip' l1 [] = []
zip' (h1:t1) (h2:t2) = (h1,h2):zip' t1 t2



-- Exercicio 9 (testa se um elemento ocorre numa lista)
elem':: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (h:t) = if n==h then True
                        else elem' n t



-- Exercicio 10 (dado um inteiro n e um elemento x constroi uma lista com n elementos, todos iguais a x.)
replicate':: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x:replicate' (n-1) x



-- Exercicio 11 (dado um elemento e uma lista, constroi uma lista em que o elemento fornecido e intercalado entre os elementos da lista fornecida.)
intersperse':: a -> [a] -> [a]
intersperse' n [] = []
intersperse' n [x] = [x]
intersperse' x (h:t) = h:x:intersperse' x t



-- Exercicio 12 (agrupa elementos iguais e consecutivos de uma lista.)                           
--group':: Eq a => [a] -> [[a]]
--group' []=[]
--group' (h:s)  =  
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h:takeWhile (== h) t) : group' (dropWhile (== h) t)


-- Exercicio 13 (concatena as listas de uma lista.)
concat':: [[a]] -> [a]
concat' [] = []
concat' ((x:xs):ys)= (x:xs) ++ concat' ys -- ((+++)(x:xs) (concat' ys)) -- usei a função que junta do exercicio 3



--Exercicio 14 (calcula a lista dos prefixos de uma lista.)
inits':: [a] -> [[a]]
inits' [] = [[]]
inits' lista = inits' (init lista) ++ [lista]


--Exercicio 15 (calcula a lista dos sufixos de uma lista.)
tails':: [a] -> [[a]]
tails' [] = [[]]
tails' (x:xs) = ((x:xs): tails' xs) 



--Exercicio 16 (que testa se uma lista e prefixo de outra.)
isPrefixOf':: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] l2 = True
isPrefixOf' l1 [] = False
isPrefixOf' l1 l2 = if head l1==head l2 then isPrefixOf' (tail l1) (tail l2)
                                        else False
        -----------ou------------
isPrefixOf'':: Eq a => [a] -> [a] -> Bool
isPrefixOf'' [] l2 = True
isPrefixOf'' l1 [] = False
isPrefixOf'' (h1:t1) (h2:t2) = if h1==h2 then isPrefixOf'' t1 t2
                                         else False



--Exercicio 17 (testa se uma lista e sufixo de outra.)
isSuffixOf':: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] l2 = True 
isSuffixOf' l1 [] = False
isSuffixOf' l1 l2 = if (last l1) == (last l2) then isSuffixOf' (init l1) (init l2)  -- (init = remove o ultimo elemento da lista, ou seja, apresenta a lista toda e remove o ultimo.)
                                              else False
 


--Exercicio 18 (testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa.)
isSubsequenceOf':: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h1:t1) (h2:t2) | h1 == h2 = isSubsequenceOf' t1 t2
                                 | otherwise = isSubsequenceOf' (h1:t1) t2



--Exercicio 19 (calcula a lista de posicoes em que um dado elemento ocorre numa lista.)
auxelemIndices':: Eq a => a -> [a] -> Int -> [Int]
auxelemIndices' n [] i = []
auxelemIndices' n (h:t) i | n==h = i:auxelemIndices' n t (i+1)
                          | otherwise = auxelemIndices' n t (i+1)
elemIndices':: Eq a => a -> [a] -> [Int]
elemIndices' n l = auxelemIndices' n l 0



--Exercicio 20 (calcula uma lista com os mesmos elementos da recebida, sem repeticoes.)
nub':: Eq a => [a] -> [a]
nub' [] = []
nub' (x:y:zs) | x==y = x:(nub' zs)
              | otherwise = nub' (y:zs) -- nao funciona, tenho que ver porque
               --- ou -----
nub'':: Eq a => [a] -> [a]
nub'' [] = []
nub'' (h:t) = h: nub'' (auxnub h t)
       where
              auxnub:: Eq a => a -> [a] -> [a]
              auxnub _ [] = []
              auxnub x (h:t) = if x==h then (auxnub x t) else (h:auxnub x t)
              ----- ou ------
nub2:: Eq a => [a] -> [a]
nub2 [] = []
nub2 (h:t) = if elem h t then nub2 t else h:nub2 t

--Exercicio 21 (retorna a lista resultante de remover (a primeira ocorrencia de) um dado elemento de uma lista.)
delete':: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t) = if x==h then t else h:delete' x t 



--Exercicio 22 ( retorna a lista resultante de remover (as primeiras ocorrˆencias) dos elementos da segunda lista da primeira.)
(\\\):: Eq a => [a] -> [a] -> [a]
(\\\) l1 [] = l1
(\\\) [] _ = []
(\\\) l1 (h:t) = ((\\\) (delete' h l1) t)



--Exercicio 23 ( retorna a lista resultante de acrescentar `a primeira lista os elementos da segunda que n˜ao ocorrem na primeira)
union' :: Eq a => [a] -> [a] -> [a]
union' l1 [] = l1
union' [] l2 = l2
union' (x:xs) (y:ys) = if elem y (x:xs) then union' (x:xs) ys --else union' (insert y (x:xs)) ys
                                        --else y:union' (x:xs) : ys
                                          else union' (x:xs ++ [y]) ys
--               | h==h1 = h1:union' t1 t2 
--               | h /= h1 = h1:t1 -------- preciso de ajuda



-- Exercicio 24 (que retorna a lista resultante de remover da primeira lista os elementos que n˜ao pertencem `a segunda.)
intersect':: Eq a => [a] -> [a] -> [a]
intersect' l1 [] = []
intersect' [] l2 = []
intersect' (h:t) l = if elem h l then h:intersect' t l    ---- atençao se fizermos psintersect     (l (h:t) = if elem h l then h : intersect l t else intersect l t )        -- vai dar mal, pois por exemplo intersect [1,1,2,3,4] [1,3,5] vai dar [1,3] e tem que dar [1,1,3]
                                   else intersect' t l
                      -- ou -- (Basicamente a função auxiliar é a função elem.)
myintersect :: Eq a => [a] -> [a] -> [a] 
myintersect [] [] = []
myintersect [] l = []
myintersect (h:t) l = if ((aux7 h l)==True) then h:myintersect t l else myintersect t l 

aux7 :: Eq a => a -> [a] -> Bool
aux7 a [] = False
aux7 a (x : xs)
               | (a == x) = True
               | otherwise = aux7 a xs 



--Exercicio 25 (dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista.)
insert':: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t) 
           | n>=h = h:insert' n t
           | n<h = n:h:t



--Exercicio 26 (junta todas as strings da lista numa so, separando-as por um espaco)
unwords':: [String] -> String
unwords' [] = ""
unwords' [x] = x                    -- Cuidado aqui, pois se nao fizermos o caso da lista singular a string vai acabar sempre com um espaço a mais--
unwords' (h:t) = h++" "++unwords' t



--Exercicio 27 (junta todas as strings da lista numa s´o, separando-as pelo caracter ’\n’)
unlines':: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t



-- Exercicio 28 (dada uma lista n˜ao vazia, retorna a posic¸˜ao onde se encontra o maior elemento da lista. As posi¸c˜oes da lista come¸cam em 0, i.e., a func¸˜ao dever´a retornar 0 se o primeiro elemento da lista for o maior.)
pMaior':: Ord a => [a] -> Int
pMaior' [x] = 0
pMaior' (h:t) = if h > head t then 0 else 1+pMaior' t
              ---ou--
pMaior'':: Ord a => [a] -> Int
pMaior'' (x:xs) = if x /= maximum (x:xs) then 1+pMaior'' xs else 0             



-- Exercicio 29 (testa se uma lista tem elementos repetidos.)
temRepetidos':: Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (h:t)
             | elem' h t == True = True
             | otherwise = temRepetidos' t



-- Exercicio 30 (determina a lista dos algarismos de uma dada lista de caracteres.)
algarismos':: [Char] -> [Char]
algarismos' [] = []
algarismos' (h:t) = if elem' h ['0'..'9'] then h:algarismos' t else algarismos' t



-- Exercicio 31 ( determina oselementos de uma lista que ocorrem em posi¸c˜oes ´ımpares. Considere que o primeiro elemento da lista ocorre na posi¸c˜ao 0 e por isso par.)
posImpares':: [a] -> [a]
posImpares' [] = []
posImpares' [x] = []
posImpares' (h:t) = (head t):posImpares' t




-- Exercicio 32 (determina os elementos de uma lista que ocorrem em posi¸c˜oes pares. Considere que o primeiro elemento da lista ocorre na posi¸c˜ao 0 e por isso par.)
posPares':: [a] -> [a]
posPares' [] = []
posPares' [x] = [x]
posPares' (h:t:ts) = h:posPares' ts



--Exercicio 33 (testa se uma lista est´a ordenada por ordem crescente.)
isSorted':: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' [x]= True
isSorted' (h:t)    |h>(head t)= False
                   |otherwise= isSorted' t



-- Exercicio 34 ( dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista)
iSort':: Ord a => [a] -> [a]
iSort' [] = []
iSort' [x] = [x]
iSort' (x:xs) = insert' x (iSort' xs)




-- Exercicio 35 (dadas duas strings, retorna True se e s´o se a primeira for menor do que a segunda, segundo a ordem lexicogr´afica (i.e., do dicion´ario))
menor':: String -> String -> Bool
menor' [] [] = False
menor' l1 [] = False
menor' [] l2 = True
menor' (h1:t1) (h2:t2) = menor' t1 t2 



-- Exercicio 36 (testa se um elemento pertence a um multi-conjunto.)
elemMSet':: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' a ((x,y):t)
                  | (a==x) =True
                  | otherwise = elemMSet' a t



-- Exercicio 37 (calcula o tamanho de um multiconjunto.)
lengthMSet':: [(a,Int)] -> Int
lengthMSet' [] = 0
lengthMSet' ((x,y):t) = y + lengthMSet' t



-- Exercicio 38 (multi-conjuto na lista dos seus elementos)
converteMSet':: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,1):t) = x:converteMSet' t
converteMSet' ((x,y):t) = x:converteMSet'((x,y-1):t)
            ----- ou ------
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((h,t):ms) = aux9 0 t h ++ converteMSet ms

aux9 :: Int -> Int -> a -> [a]
aux9 x y a
           | (x < y) = a : aux9 (x+1) y a
           | otherwise = []



-- Exercicio 39 (acrescenta um elemento a um multi-conjunto.)
insereMSet':: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' n [] = [(n,1)]
insereMSet x ((a,n):xs) = if x == a then (a,n+1):xs else (a,n):insereMSet x xs




-- Exercicio 40 (que remove um elemento a um multi-conjunto. Se o elemento n˜ao existir, deve ser retornado o multi-conjunto recebido)
removeMSet':: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' x [] = []
removeMSet' x ((a,0):xs) = removeMSet' x xs
removeMSet' x ((a,n):xs) = if x == a then (a,n-1):xs else (a,n):removeMSet' x xs

                   --- Ou ---
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((h,t) : ms)
                         |((a == h) && ( t > 1)) = (h,t-1) : removeMSet a ms
                         |((a == h) && (t == 1)) = removeMSet a ms
                         |otherwise = (h,t) : removeMSet a ms




-- Exercicio 41 (dada uma lista ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos.)
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' [x] = [(x,1)]
constroiMSet' l = aux41 l 1

aux41 (h:x:t) n = if h == x then aux41 (x:t) (n+1) else (h,n):aux41 (x:t) 1
aux41 (x:t) n = [(x,n)]
aux41 _ n = []


constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet [x] = [(x,1)]
constroiMSet (l:ls) = insereMSet' l (constroiMSet ls)


-- Exercicio 42 

listaEithers = [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]

partitionEithers' ::  [Either a b] -> ([a],[b])
partitionEithers' l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right x):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left x):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls



-- Exercicio 43
catMaybes':: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just a): ms) = a:catMaybes' ms
catMaybes' ((Nothing):ms) = catMaybes' ms



--Exercicio 44 ( calcula a posi¸c˜ao final do robot depois de efectuar essa sequˆencia de movimentos.)
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

getMov ::  Movimento -> String
getMov Norte = "Norte"
getMov Sul   = "Sul"
getMov Este  = "Este"
getMov Oeste = "Oeste"

posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (m:ms) | (getMov m) == "Norte" = posicao (x,y+1) ms
                     | (getMov m) == "Sul"   = posicao (x,y-1) ms
                     | (getMov m) == "Este"  = posicao (x+1,y) ms
                     | (getMov m) == "Oeste" = posicao (x-1,y) ms




-- Exercicio 45 (inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o robot passe de uma posi¸c˜ao para a outra)
caminho:: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) = movimentohorizontal xi xf ++ movimentovertical yi yf
  where
    movimentohorizontal y1 y2 | y1 > y2  = Oeste : movimentohorizontal (y1-1) y2
                              | y1 < y2  = Este : movimentohorizontal (y1+1) y2
                              | y1 == y2 = []
    movimentovertical x1 x2 | x1 > x2  = Sul : movimentovertical (x1-1) x2
                            | x1 < x2  = Norte : movimentovertical (x1+1) x2
                            | x1 == x2 = []



-- Exercicio 46 ( testa se uma lista de movimentos s´o ´e composta por movimentos verticais (Norte ou Sul).)
vertical:: [Movimento] -> Bool
vertical [Norte] = True
vertical [Sul] = True
vertical [] = False
vertical (h:t)= if getMov h == "Norte"  || getMov h == "Sul" then vertical t else False



--Exercicio 47 ( determina a que est´a mais perto da origem (note que as coordenadas de cada ponto s˜ao n´umeros inteiros).)
data Posicao = Pos Int Int
               deriving Show

maisCentral:: [Posicao] -> Posicao
maisCentral (x:xs) = centro xs x
  where
    centro [] solucao = solucao
    centro ((Pos a b):ys) solucao@(Pos c d) = if (a + b) <= (c + d)
      then centro ys (Pos a b)
      else centro ys solucao



-- Exercicio 48 (dada uma posi¸c˜ao e uma lista de posi¸c˜oes, selecciona da lista as posi¸c˜oes adjacentes `a posi¸c˜ao dada)
vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos a b) list = [(Pos c d) | (Pos c d) <- list, c >= (a-1) && c <= (a+1) && d >= (b-1) && d <= (b+1)]
                          ----- ou -------- (Formas que o david Fez)
vizinhos' :: Posicao -> [Posicao] -> [Posicao]
vizinhos' _ [] = []
vizinhos' (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
  then (Pos xv yv):vizinhos' (Pos x y) ps 
  else vizinhos' (Pos x y) ps



-- Exercicio 49 ( testa se todas as posi¸c˜oes de uma dada lista tˆem a mesma ordenada.)
mesmaOrdenada ::  [Posicao] -> Bool
mesmaOrdenada ((Pos x a):[]) = True
mesmaOrdenada ((Pos x a):(Pos y b):xs) = if a == b
  then mesmaOrdenada ((Pos y b):xs)
  else False



-- Exercicio 50 (testa se o estado dos sem´aforos de um cruzamento ´e seguro, i.e., n˜ao h´a mais do que sem´aforo n˜ao vermelho)
data Semaforo = Verde | Amarelo | Vermelho
                  deriving Show

luz:: Semaforo -> String
luz Vermelho = "Vermelho"
luz Amarelo = "Amarelo"
luz Verde = "Verde"

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK lista = length [(Vermelho) | (Vermelho) <- lista] == 2


--End













-- Cenas que me vao na cabeça
init':: [a] -> [a]
init' [] = []
init' [a] = []
init' (h:t) = h:init' t 




posimpares:: [a] -> [a]
posImpares [] = []
posimpares [x] = []
posimpares (h:t:ts) = t:posimpares ts

posImpares'' ::  [a] -> [a]
posImpares'' [] = []
posImpares'' [x] = []
posImpares''(x : xs) = (head xs) : posImpares'' (tail xs)
