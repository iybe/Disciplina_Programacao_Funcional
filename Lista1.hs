-- Lista 1 de Ricardo Reis de 2014

--    ______________    _____________    ______________   ___            _____________   ___      ___ 
--    \_____   _____\   \   _________\   \   __________\  \  \           \   _________\  \  \    /  /
--          \  \         \  \             \  \             \  \           \  \            \  \  /  /
--           \  \         \  \______       \  \___________  \  \           \  \______      \  \/  /
--            \  \         \   _____\       \___________  \  \  \           \   _____\      \    /
--             \  \         \  \                        \  \  \  \           \  \            \   \
--        ______\  \_____    \  \__________    __________\  \  \  \_________  \  \__________  \   \ 
--        \______________\    \____________\   \_____________\  \___________\  \____________\  \___\

--1 - menordedois
--INPUT: Dois números, x e y
--OUTPUT: menor valor entre x e y

menorDeDois :: Int -> Int -> Int
menorDeDois a b | a < b = a
                | otherwise = b

--2. menorDeTres
--INPUT: Três números, x, y e z
--OUTPUT: menor valor entre x, y e z

menorDeTres :: Int -> Int -> Int -> Int
menorDeTres a b c | (a <= b) && (a <= c) = a
                  | (b <= a) && (b <= c) = b
                  | otherwise = c

--3. fatorial
--INPUT: Um natural n
--OUTPUT: O fatorial de n

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = fatorial(n - 1) * n

--4. fibonacci
--INPUT: Inteiro positivo n
--OUTPUT: n-ésimo termo da sequência de Fibonacci(iniciando em com 0 e 1)

fibonacci :: Int -> Int
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

--5. elemento
--INPUT: Lista u e um natural n
--OUTPUT: n-ésimo termo de u

elemento :: [Int] -> Int -> Int
elemento (p:l) n | n == 1 = p
                 | otherwise = elemento l (n-1)

--6. pertence
--INPUT: Lista u e valor x
--OUTPUT: Verdadeiro se x esta em u e falso do contrário

pertence :: [Int] -> Int -> Bool
pertence [] _ = False
pertence (p:u) x | p == x = True
                 | otherwise = pertence u x

--7. total
--INPUT: Lista u
--OUTPUT: total de elementos de u.

total :: [Int] -> Int
total [] = 0
total (p:u) = 1 + total u 

--8. maior
--INPUT: Lista u
--OUTPUT: A chave de valor máximo em u.

maior :: [Int] -> Int
maior (p:u) | u == [] = p
            | maior(u) <= p = p
            | otherwise = maior(u) 

--9. frequencia
--INPUT: Lista u e valor x
--OUTPUT: Retorna o total de ocorrências de x em u.

frequencia :: [Int] -> Int -> Int
frequencia [] _ = 0
frequencia (p:u) x | p == x = 1 + frequencia u x
                   | otherwise = frequencia u x

--10. unico
--INPUT: Lista u e valor x
--OUTPUT: Verdadeiro se x ocorre exatamente uma vez em u e falso do contrário

unico :: [Int] -> Int -> Bool
unico [] _ = False
unico (p:u) x | (x == p) && ((frequencia u p) == 0) = True
              | x == p = False
              | otherwise = unico u x

--11. maioresQue
--INPUT: Número x e uma lista u de números
--OUTPUT: Sublista de u cujos números sejam maiores que x

maioresQue :: Int -> [Int] -> [Int]
maioresQue _ [] = []
maioresQue x (p:u) | p > x = p:(maioresQue x u)
                   | otherwise = maioresQue x u

--12. concat
--INPUT: Duas listas a e b
--OUTPUT: Concatenação entre a e b

conca :: [Int] -> [Int] -> [Int]
conca a b = a ++ b

--13. calda
--INPUT: Uma lista u
--OUTPUT: Calda de u (u sem a primeira chave)

calda :: [Int] -> [Int]
calda (p:u) = u

--14. corpo
--INPUT: Uma lista u
--OUTPUT: Corpo de u (u sem a última chave)

corpo :: [Int] -> [Int]
corpo (p:u) | u == [] = []
            | otherwise = p:corpo(u)

--15. unique
--INPUT: Lista u com possíveis chaves repetidas
--OUTPUT: Lista com as chaves de u sem repetições

unique :: [Int] -> [Int]
unique (p:u) | u == [] = [p]
             | frequencia u p == 0 = p:(unique u)
             | otherwise = unique u

--16. menores
--INPUT: Natural n e lista u
--OUTPUT: Lista com os n menores elementos de u na ordem que aparecem em u
-- 3 [7,5,9,1,6,12,0]
menores :: Int -> [Int] -> [Int]
menores n u = [x | x <- u, (length (maioresQue x u)) >= ((length u) - n)]

--17. alter
--INPUT: Inteiro n
--OUTPUT: Lista [1;-1; 2;-2; 3;-3; ... ; n;-n]

alter :: Int -> [Int]
alter n | n == 1 = [1, -1]
        | otherwise = (alter(n - 1))++[n, -n]

--18. reverso
--INPUT: Lista u
--OUTPUT: Lista das chaves de u na ordem inversa.
--EX(S): reverso [1,2,3,4] ==> [4,3,2,1]

reverso :: [Int] -> [Int]
reverso (p:u) | u == [] = [p]
              | otherwise = reverso(u)++[p]

--19. divide
--INPUT: Lista u e um natural n
--OUTPUT: Tupla de duas listas, (A;B), onde A é formada pelas n
--primeiras chaves de u e B pelos elementos restantes 
divide :: [Int] -> Int -> ([Int],[Int])
divide u n = (take n u , drop n u)

--20. intercal
--INPUT: Duas listas a e b
--OUTPUT: Lista com os elementos de a e b intercalados

intercal :: [Int] -> [Int] -> [Int]
intercal (p:a) (s:b) | a == [] || b == [] = [p,s]++a++b
                     | otherwise = [p,s]++intercal a b 

--21. uniao
--INPUT: Duas listas a e b sem repetição de chaves
--OUTPUT: Lista das chaves de a e b sem repetição

uniao :: [Int] -> [Int] -> [Int]
uniao (p:a) (s:b) | a == [] || b == [] = [p,s]++a++b
                  | ((frequencia (a++b) p) == 0) && ((frequencia (a++b) s) == 0) = [p,s]++(uniao a b)
                  | ((frequencia (a++b) p) == 0) && not((frequencia (a++b) s) == 0) = [p]++(uniao a b)
                  | not((frequencia (a++b) p) == 0) && ((frequencia (a++b) s) == 0) = [s]++(uniao a b)
                  | otherwise = uniao a b

--22. intersec
--INPUT: Duas listas a e b sem repetição de chaves
--OUTPUT: Lista das chaves que a e b possuem em comum

intersec :: [Int] -> [Int] -> [Int]
intersec [] _ = []
intersec (p:a) b | frequencia b p > 0 = [p]++(intersec a b)
                 | otherwise = intersec a b 

--23. sequencia
--INPUT: Dois números naturais n e m
--OUTPUT: Lista [m;m + 1;m + 2; ... ;m + n - 1]

sequencia :: Int -> Int -> [Int]
sequencia n m = [m,(m+1)..(m+n-1)]

--24. inserir
--INPUT: Número x e lista, u, de números ordenados ascendentemente
--OUTPUT: Lista de números ordenados ascendentemente,
--oriunda da inserção apropriada de x em u
--inserir 3 [2,7,12] ==> [2,3,7,12]

inserir :: Int -> [Int] -> [Int]
inserir _ [] = []
inserir x (p:u) | x > p = [p]++(inserir x u)
                | otherwise = [x,p]++u

--25. isSorted
--INPUT: Lista de números, u
--OUTPUT: Verdadeiro se u é ordenada e falso do contrário

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (p:u) | u == [] = True
               | p <= (u !! 0) = isSorted u
               | otherwise = False

--26. qsort
--INPUT: Lista u de objetos ordenáveis
--OUTPUT: Lista ordenada das chaves de u pelo método de ordenação rápida

menoresq :: Int -> [Int] -> [Int]
menoresq pi [] = []
menoresq pi (p:l) | p <= pi = [p]++(menoresq pi l)
                  | otherwise = (menoresq pi l)

maiorque :: Int -> [Int] -> [Int]
maiorque pi [] = []
maiorque pi (p:l) | p > pi = [p]++(maiorque pi l)
                  | otherwise = (maiorque pi l)

opivo :: [Int] -> Int
opivo l = (l !! (div (length l) 2))

listasemp :: [Int] -> Int -> [Int]
listasemp [] _ = []
listasemp (pr:l) p | pr == p = l
                   | otherwise = [pr]++(listasemp l p)

qsort :: [Int] -> [Int]
qsort l | ((length l) == 0) || ((length l) == 1) = l
        | otherwise = (qsort ( menoresq (opivo l) (listasemp l (opivo l)) ))++[(opivo l)]++(qsort (maiorque (opivo l) (listasemp l (opivo l)) ))

--27. rotEsq
--INPUT: Um natural n e uma lista ou string S
--OUTPUT: Lista S rotacionada n vezes à esquerda

rotEsq :: Int -> [Char] -> [Char]
rotEsq n s = (drop n s)++(take n s)

--28. rotDir
--INPUT: Um natural n e uma lista ou string S
--OUTPUT: Lista S rotacionada n vezes à direita

rotDir :: Int -> [Char] -> [Char]
rotDir n s = (drop ((length s) - n) s)++(take ((length s) - n) s)

--29. upper
--INPUT: Uma string S
--OUTPUT: Versão em caixa alta da string S

indice :: Char -> Int -> [Char] -> Int
indice _ _ [] = -1
indice c n (p:l) | c == p = n
                 | otherwise = (indice c (n+1) l)

maiuscula :: Char -> Char
maiuscula c = ['A'..'Z'] !! (indice c 0 ['a'..'z']) 

upper :: [Char] -> [Char]
upper s = [if a `elem` ['a'..'z'] then (maiuscula a) else a | a <- s]

--30. titulo
--INPUT: String S
--OUTPUT: Versão de S contendo todos os caracteres
--em caixa baixa exceto aqueles que, por serem
--iniciais de palavras, devem aparecer em caixa alta

minusculo :: Char -> Char
minusculo c = ['a'..'z'] !! (indice c 0 ['A'..'Z'])

down :: [Char] -> [Char]
down s = [if a `elem` ['A'..'Z'] then (minusculo a) else a | a <- s]

solvetitle :: [Char] -> Int -> Char -> [Char]
solvetitle [] _ _ = []
solvetitle (p:l) ia ca | ia == 0 = (upper [p])++(solvetitle l (ia+1) p)
                       | (ca == ' ') && (p /= ' ') = (upper [p])++(solvetitle l (ia+1) p)
                       | (ca /= ' ') = (down [p])++(solvetitle l (ia+1) p)
                       | otherwise = [p]++(solvetitle l (ia+1) p)

titulo :: [Char] -> [Char]
titulo s = solvetitle s 0 ' '

--31. selec
--INPUT: Uma lista qualquer u e uma lista de posições P
--OUTPUT: Lista das chaves de u cujas posições estão em P
selec :: [Char] -> [Int] -> [Char]
selec u p = [a | b <- p, a <- [u !! b]] 

--32. isPalind
--INPUT: Uma string qualquer S
--OUTPUT: Verdadeiro se S é um palíndromo e falso do contrário
isPalind :: [Char] -> Bool
isPalind [] = True
isPalind (p:s) | length s == 0 = True
               | p == (last s) = isPalind (init s)
               | otherwise = False

-- 33

primo :: Int -> Bool
primo 1 = False
primo n = ((length [c | c <- [2..(n-1)],(mod n c) == 0] ) == 0)

-- 34

sdig :: Int -> Int
sdig 0 = 0
sdig n = (n-((div n 10)*10))+(sdig (div n 10))

-- 35

nprim :: Int -> [Int] -> [Int]
nprim _ [] = []
nprim n (p:l) | n == 0 = []
              | otherwise = [p]++(nprim (n-1) l)

nult :: Int -> [Int] -> [Int]
nult _ [] = []
nult n (p:l) | n == 0 = []
             | n > length l = p:l
             | n < length l = nult n l
             | otherwise = l

valornaposicao :: Int -> Int -> [Int] -> [Int]
valornaposicao _ _ [] = []
valornaposicao v pos (p:l) | pos == 0 = [v]++l
                           | otherwise = [p]++(valornaposicao v (pos-1) l)

swap :: [Int] -> Int -> Int -> [Int]
swap l a b = (valornaposicao (l !! (max a b)) (min a b) (nprim ((min a b)+1) l) )++(valornaposicao (l !! (min a b)) ((max a b)-((min a b)+1)) (nult ((length l)-(min a b)-1) l) )

valorindmenor :: [Int] -> Int -> Int -> Int -> [Int]
valorindmenor [] v im _ = [v,im]
valorindmenor (p:l) v im ia | p < v = (valorindmenor l p ia (ia+1))
                            | otherwise = (valorindmenor l v im (ia+1))

solvebubble :: [Int] -> Int -> [Int]
solvebubble [] _ = []
solvebubble l ia = [((valorindmenor l (l !! 0) ia ia) !! 0)]++(solvebubble (nult ((length l)-1) (swap l (ia) ((valorindmenor l (l !! 0) ia ia) !! 1))) (ia))

bubblesort :: [Int] -> [Int]
bubblesort l = solvebubble l 0

-- 36

solvecompac :: Int -> [Int] -> [[Int]]
solvecompac ac (p:[]) | ac == 0 = [[p]]
                      | otherwise = [[ac+1,p]]
solvecompac ac (p:l) | (l !! 0) == p = (solvecompac (ac+1) l)
                     | ac == 0 = [[p]]++(solvecompac 0 l)
                     | otherwise = ([[ac+1,p]]++(solvecompac 0 l))

compac :: [Int] -> [[Int]]
compac l = (solvecompac 0 l)

-- 37
listapares :: [Int] -> [Int]
listapares [] = []
listapares (p:l) | (mod p 2) == 0 = [p]++(listapares l)
                 | otherwise = (listapares l) 

listaimpares :: [Int] -> [Int]
listaimpares [] = []
listaimpares (p:l) | not ((mod p 2) == 0) = [p]++(listaimpares l)
                   | otherwise = (listaimpares l)

splitlints :: [Int] -> [[Int]]
splitlints l = [listapares l]++[listaimpares l]

-- 38

perfeito :: Int -> Bool
perfeito n = (n `elem` [(e^2) | e <- [1..n], (e^2) `elem` [1..n]])

-- 39

numnabase :: Int -> Char
numnabase n = (['0'..'9']++['A'..'Z']) !! n

primpotmqn :: Int -> Int -> Int -> Int
primpotmqn n ba e | (ba^e) > n = (ba^(e-1))
                  | otherwise = (primpotmqn n ba (e+1))

baseprimpotmqn :: Int -> Int -> Int -> Int
baseprimpotmqn n ba e | (ba^e) > n = (e-1)
                      | otherwise = (baseprimpotmqn n ba (e+1))

repete :: Int -> Char -> [Char]
repete n c | n == 0 = []
           | otherwise = [c]++(repete (n-1) c)

sbase :: Int -> Int -> Int -> [Char]
sbase 0 _ e = (repete (e+1) '0')
sbase n b e | (b^e) > n = ['0']++(sbase n b (e-1))
            | otherwise = [(numnabase (div n (primpotmqn n b 0)))]++(sbase (mod n (primpotmqn n b 0)) b (e-1))

base :: Int -> Int -> [Char]
base n b = sbase n b (baseprimpotmqn n b 0)

-- 40
gerarsub :: [Char] -> [Int] -> [Int]
gerarsub [] _ = []
gerarsub (p:r) (pi:l) | p == '1' = [pi]++(gerarsub r l)
                      | otherwise = (gerarsub r l)

gerartdsubs :: Int -> [Int] -> [[Int]]
gerartdsubs na l | na == (2^(length l)) = []
                 | otherwise = [(bubblesort (gerarsub (( repete ((length l)-(length (base na 2))) '0' )++(base na 2)) l))]++(gerartdsubs (na+1) l)

existel :: [Int] -> [[Int]] -> Bool
existel _ [] = False
existel p (o:l) | p == o = True
                | otherwise = (existel p l)

retiraiguais :: [[Int]] -> [[Int]]
retiraiguais [] = []
retiraiguais (p:l) | (existel p l) = (retiraiguais l)
                   | otherwise = [p]++(retiraiguais l) 

partes :: [Int] -> [[Int]]
partes l = (retiraiguais (gerartdsubs 0 l))

