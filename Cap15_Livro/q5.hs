import System.IO

ajeita :: [Int] -> String
ajeita (x:[]) = (show x)
ajeita (x:xs) = (show x)++"@"++(ajeita xs)

escreverLista :: [Int] -> IO()
escreverLista lista = do
    arq <- openFile "arquivo.txt" WriteMode
    hPutStr arq (ajeita lista)
    hFlush arq
    hClose arq