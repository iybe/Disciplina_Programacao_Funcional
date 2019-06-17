import System.IO

convert :: String -> String -> [Int]
convert [] acum = [read acum :: Int]
convert (x:xs) acum | x == '@' = [read acum :: Int]++(convert xs [])
                    | otherwise = (convert xs (acum++[x]))

main = do
    arq <- openFile "arquivo.txt" ReadMode
    s <- hGetContents arq
    print (convert s [])
    print (sum (convert s []))
    hClose arq