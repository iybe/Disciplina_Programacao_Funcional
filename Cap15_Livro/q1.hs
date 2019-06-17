import Data.Char

main :: IO()
main = do
    ent <- getLine
    if (valido ent)
        then print (reverso ent)
        else
            print (erro ent)

valido :: [Char] -> Bool
valido [] = True
valido (x:xs) | isAlpha x = (valido xs)
              | otherwise = False

reverso :: [Char] -> [Char]
reverso [] = []
reverso (x:xs) = (reverso xs)++[x]

erro :: [Char] -> [Char]
erro [] = "  Erro"
erro (' ':xs) = "  Erro"
erro (x:xs) | not (isAlpha x) = ([x]++" Erro")