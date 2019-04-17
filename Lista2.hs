-- lista 2

--    ______________    _____________    ______________   ___            _____________   ___      ___ 
--    \_____   _____\   \   _________\   \   __________\  \  \           \   _________\  \  \    /  /
--          \  \         \  \             \  \             \  \           \  \            \  \  /  /
--           \  \         \  \______       \  \___________  \  \           \  \______      \  \/  /
--            \  \         \   _____\       \___________  \  \  \           \   _____\      \    /
--             \  \         \  \                        \  \  \  \           \  \            \   \
--        ______\  \_____    \  \__________    __________\  \  \  \_________  \  \__________  \   \ 
--        \______________\    \____________\   \_____________\  \___________\  \____________\  \___\

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

-- 5
cond_a :: [Int] -> Int -> Int
cond_a (p:l) ia | l == [] = -1
                | p > (l !! 0) = ia
                | otherwise = (cond_a l (ia-1))

abs_cond_a :: [Int] -> Int
abs_cond_a l = (cond_a (reverse l) ((length l) - 2))

cond_b :: Int -> [Int] -> Int -> Int
cond_b val (p:l) ia | p > val = ia
                    | otherwise = (cond_b val l (ia-1))

abs_cond_b :: Int -> [Int] -> Int
abs_cond_b i u = (cond_b (u !! i) (reverse ((nult ((length u)-i-1)) u)) ((length u)-1) )

cond_d :: [Int] -> Int -> [Int]
cond_d u i = ((nprim (i+1) u)++(reverse (nult ((length u)-i-1) u)))

nextPerm :: [Int] -> [Int]
nextPerm l = (cond_d (swap l (abs_cond_a l) (abs_cond_b (abs_cond_a l) l)) (abs_cond_a l) )

-- 6

allPerms :: [Int] -> [[Int]]
allPerms l | (abs_cond_a l) == -1 = [l]
           | otherwise = ([l]++(allPerms (nextPerm l))) 

-- 7

bbin :: [Int] -> Int -> Int -> Int
bbin [] _ _ = -1
bbin l v ac | (l !! (div (length l) 2)) == v = (ac+(div (length l) 2))
            | (l !! (div (length l) 2)) > v = (bbin (nprim (div (length l) 2) l) v ac )
            | otherwise = (bbin (nult ((length l)-(div (length l) 2)-1) l) v (ac+((div (length l) 2)+1) ))

buscabin :: [Int] -> Int -> Int
buscabin l v = (bbin l v 0)

-- 8

primo :: Int -> Bool
primo 1 = False
primo n = ((length [c | c <- [2..(n-1)],(mod n c) == 0] ) == 0)

lp :: Int -> [Int]
lp n = [p | p <- [1..n],primo p]

qtdvezdiv :: Int -> Int -> Int -> Int
qtdvezdiv 0 _ _ = 0
qtdvezdiv n di q | ((mod n di) /= 0) = q
                 | otherwise = (qtdvezdiv (div n di) di (q+1))

solvefact :: Int -> [Int] -> [[Int]]
solvefact _ [] = []
solvefact 0 _ = []
solvefact n (p:l) | (qtdvezdiv n p 0) == 0 = (solvefact n l)
                  | otherwise = [[p,(qtdvezdiv n p 0)]]++(solvefact (div n (p^( qtdvezdiv n p 0))) l) 

factors :: Int -> [[Int]]
factors n = (solvefact n (lp n))

-- 9
listaacum :: [Int] -> [Int]
listaacum [] = []
listaacum (p:l) = ([p+(sum l)]++(listaacum l)) 

listacc :: [Int] -> [Int]
listacc l = (reverse (listaacum (reverse l))) 

-- 10
listamax :: [Int] -> Int -> [Int] -> [Int] -> [Int]
listamax [] _ lm _ = lm
listamax (p:l) va lm lac | (va + p) > (sum lm) = (listamax l (va+p) (lac++[p]) (lac++[p]))
                         | p > 0 = (listamax l (va+p) lm (lac++[p]))
                         | (va+p) > 0 = (listamax l (va+p) lm (lac++[p]))
                         | otherwise = (listamax l 0 lm [])

maxsseq :: [Int] -> [Int]
maxsseq l = (listamax l 0 [] [])
