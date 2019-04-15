-- 1 
paridade :: [Bool] -> Bool
paridade [] = False
paridade (p:l) | p = (True && not (paridade l))
               | otherwise = (paridade l)

-- 2
listaparainteiro :: [Int] -> Int
listaparainteiro [] = 0
listaparainteiro (p:l) = ((p*(10^(length l))) + (listaparainteiro l))

inteiroparalistareversa :: Int -> [Int]
inteiroparalistareversa 0 = []
inteiroparalistareversa n = ([n - ((div n 10)*10)] ++ (inteiroparalistareversa (div n 10)))
 
rev :: Int -> Int
rev n = listaparainteiro (inteiroparalistareversa n)

-- 3

delete' :: Int -> [Int] -> [Int]
delete' _ [] = []
delete' n (p:l) | n == p = l
                | otherwise = [p]++(delete' n l) 

-- 4

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

-- 7

bbin :: [Int] -> Int -> Int -> Int
bbin [] _ _ = -1
bbin l v ac | (l !! (div (length l) 2)) == v = (ac+(div (length l) 2))
            | (l !! (div (length l) 2)) > v = (bbin (nprim (div (length l) 2) l) v ac )
            | otherwise = (bbin (nult ((length l)-(div (length l) 2)-1) l) v (ac+((div (length l) 2)+1) ))

buscabin :: [Int] -> Int -> Int
buscabin l v = (bbin l v 0)

-- 9
listaacum :: [Int] -> [Int]
listaacum [] = []
listaacum (p:l) = ([p+(sum l)]++(listaacum l)) 

listacc :: [Int] -> [Int]
listacc l = (reverse (listaacum (reverse l))) 