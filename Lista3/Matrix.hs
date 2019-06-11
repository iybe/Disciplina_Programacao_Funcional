type Row = [Float]
data Matrix = Matrix {ncols :: Int , nrows :: Int , rows :: [Row]} deriving (Show)

--zeroMatrix
zeroMatrix :: Int -> Int -> Matrix
zeroMatrix colunas linhas = Matrix {ncols = colunas, nrows = linhas, rows = (matrixZeros colunas linhas)}

matrixZeros :: Int -> Int -> [Row]
matrixZeros colunas 0 = []
matrixZeros colunas linhas = [(repete 0 colunas)]++(matrixZeros colunas (linhas-1))

repete :: Float -> Int -> Row
repete num 0 = []
repete num n = [num]++(repete num (n-1))

--oneMatrix
oneMatrix :: Int -> Int -> Matrix
oneMatrix colunas linhas = Matrix {ncols = colunas, nrows = linhas, rows = (matrixuns colunas linhas)}

matrixuns :: Int -> Int -> [Row]
matrixuns colunas 0 = []
matrixuns colunas linhas = [(repete 1 colunas)]++(matrixuns colunas (linhas-1))

--identMatrix
identMatrix :: Int -> Matrix
identMatrix qtd = Matrix {ncols = qtd, nrows = qtd, rows = (identidade 0 qtd)}

identidade :: Int -> Int -> [Row]
identidade pa qtd | (pa+1) == qtd = [(coloca 1 pa 0 (repete 0 qtd))]
                  | otherwise = [(coloca 1 pa 0 (repete 0 qtd))]++(identidade (pa+1) qtd)

coloca :: Float -> Int -> Int -> Row -> Row
coloca _ _ _ [] = []
coloca valor pos ind (x:xs) | pos == ind = [valor]++xs
                            | otherwise = [x]++(coloca valor pos (ind+1) xs)

--sumMatrix
sumMatrix :: Matrix -> Matrix -> Matrix
sumMatrix (Matrix {ncols = cols, nrows = linhas,rows = m1}) (Matrix {ncols = _,nrows =  _,rows = m2}) = Matrix {ncols = cols, nrows = linhas, rows = somaMatriz m1 m2}

somaMatriz :: [Row] -> [Row] -> [Row]
somaMatriz [] [] = []
somaMatriz (x:xs) (y:ys) = [somaLista x y]++(somaMatriz xs ys) 

somaLista :: Row -> Row -> Row
somaLista [] [] = []
somaLista (x:xs) (y:ys) = [x+y]++(somaLista xs ys)

--prodScalar
prodScalar :: Float -> Matrix -> Matrix
prodScalar valor (Matrix {ncols = cols, nrows = linhas, rows = m1}) = Matrix {ncols = cols, nrows = linhas, rows = (prod valor m1)}

prod :: Float -> [Row] -> [Row]
prod _ [] = []
prod valor (x:xs) = [(map (valor*) x)]++(prod valor xs)

--prodMatrix
prodMatrix :: Matrix -> Matrix -> Matrix
prodMatrix (Matrix {ncols = _, nrows = nl, rows = m1}) (Matrix {ncols = nc, nrows = ql, rows = m2}) = resp
        where resp = Matrix {ncols = nc, nrows = nl, rows = (prodM m1 0 (linhacoluna m2 0 ql))}

prodM :: [Row] -> Int -> [Row] -> [Row]
prodM [] _ _ = []
prodM (x:xs) ia m = [(gerarlinha x m)]++(prodM xs (ia+1) m)

gerarlinha :: Row -> [Row] -> Row
gerarlinha lin m = (map sum (map (multm lin) m))

multm :: Row -> Row -> Row
multm [] [] = []
multm (x:xs) (y:ys) = [x*y]++(multm xs ys)

linhacoluna :: [Row] -> Int -> Int -> [Row]
linhacoluna m ia n | ia == n = []
                   | otherwise = [(colunax m ia)]++(linhacoluna m (ia+1) n)

colunax :: [Row] -> Int -> Row
colunax [] _ = []
colunax (x:xs) ia = [x !! ia]++(colunax xs ia)

--listToMatrix
listToMatrix :: [Row] -> Matrix
listToMatrix (x:xs) = Matrix (length x) (1+(length xs)) ([x]++xs)