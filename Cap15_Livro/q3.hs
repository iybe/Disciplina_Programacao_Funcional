main = do
    putStrLn "Digite uma operacao valida"
    op <- getLine
    if (resp op) && (tem op) && (ve op)
        then putStrLn "Expressao Valida!"
        else putStrLn "Expressao Invalida!"

ve :: [Char] -> Bool
ve xs = (estaEm (xs !! 0) ['0'..'9']) && (estaEm (xs !! ((length xs)-1)) ['0'..'9'])

tem :: [Char] -> Bool
tem [] = False
tem (x:xs) | x == '+' = True
           | x == '-' = True
           | x == '*' = True
           | x == '/' = True
           | otherwise = (tem xs)

ateC :: [Char] -> Char -> [Char]
ateC [] _ = []
ateC (x:xs) c | x == c = []
              | otherwise = [x]++(ateC xs c)

reverso :: [Char] -> [Char]
reverso [] = []
reverso (x:xs) = (reverso xs)++[x]

cheko :: [Char] -> Bool
cheko [] = True
cheko (x:xs) | (estaEm x ['0'..'9']) = (cheko xs)
             | otherwise = False

estaEm :: Char -> [Char] -> Bool
estaEm _ [] = False
estaEm v (x:xs) | v == x = True
                | otherwise = (estaEm v xs)

resp :: [Char] -> Bool
resp xs = e1 || e2 || e3 || e4
        where e1 = ((cheko (ateC xs '+')) && (cheko (ateC (reverso xs) '+')))
              e2 = ((cheko (ateC xs '-')) && (cheko (ateC (reverso xs) '-')))
              e3 = ((cheko (ateC xs '*')) && (cheko (ateC (reverso xs) '*')))
              e4 = ((cheko (ateC xs '/')) && (cheko (ateC (reverso xs) '/')))