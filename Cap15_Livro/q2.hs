main = do
    ent <- getLine
    if ent == []
        then return ()
        else do
            putStrLn (ajeita ent)
            main

ajeita :: String -> String
ajeita (x:xs) | ind /= -1 = [(['A'..'Z'] !! ind)]++xs
              | otherwise = ([x]++xs)
            where ind = (indice 0 x ['a'..'z'])

indice :: Int -> Char -> [Char] -> Int
indice _ _ [] = -1
indice id c (x:xs) | c == x = id
                   | otherwise = (indice (id+1) c xs)