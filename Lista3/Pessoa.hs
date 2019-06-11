data Pessoa = Pessoa {nome :: String, idade :: Int, salario :: Float} deriving (Show)

data Criterio = ByNome | ByIdade | BySalario deriving (Eq)

sortListPessoa :: [Pessoa] -> Criterio -> [Pessoa]
sortListPessoa [] _ = []
sortListPessoa (x:xs) crit = ladoE++[x]++ladoD
    where ladoE = [a | a <- xs,(comp1 a x crit)]
          ladoD = [a | a <- xs,(comp2 a x crit)]

comp1 :: Pessoa -> Pessoa -> Criterio -> Bool
comp1 p1 p2 crit | crit == ByNome = ((nome p1) <= (nome p2))
                 | crit == ByIdade = ((idade p1) <= (idade p2))
                 | otherwise = ((salario p1) <= (salario p2))

comp2 :: Pessoa -> Pessoa -> Criterio -> Bool
comp2 p1 p2 crit | crit == ByNome = ((nome p1) > (nome p2))
                 | crit == ByIdade = ((idade p1) > (idade p2))
                 | otherwise = ((salario p1) > (salario p2))