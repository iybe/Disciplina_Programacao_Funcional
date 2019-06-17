leInt :: [Int] -> IO [Int]
leInt l = do
    ent <- getLine
    if ent == "0"
        then return (l)
        else do
            r <- (leInt (l++[read ent :: Int]))
            return r

le_lista_int :: IO [Int]
le_lista_int = leInt []