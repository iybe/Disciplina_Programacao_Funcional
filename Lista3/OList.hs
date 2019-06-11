data OList a = Empty | Node a (OList a) deriving (Show,Eq)

(>>>) :: (Ord a) => a -> OList a -> OList a
(>>>) valor Empty = Node valor (Empty)
(>>>) valor (Node atual resto) | valor <= atual = Node valor (Node atual resto)
                               | otherwise = Node atual ((>>>) valor resto)

--wtf!
hasKey :: (Ord a) => a -> OList a -> Bool
hasKey _ Empty = False
haskey valor (Node p resto) | p == valor = True
                            | resto == Empty = False
                            | otherwise = (haskey valor resto) 

remkey :: (Ord a) => a -> OList a -> OList a
remkey _ Empty = Empty
remkey chave (Node p resto) | chave == p = resto
                            | otherwise = Node p (remkey chave resto)

key :: Int -> OList a -> Maybe a
key _ Empty = Nothing
key n (Node p resto) | n == 0 = Just p
                     | otherwise = (key (n-1) resto)