data Stack a = Empty | Top a (Stack a) deriving(Show)

push :: a -> Stack a -> Stack a
push valor pilha = Top valor pilha

pop :: Stack a -> Stack a
pop (Top valor pilha) = pilha

height :: Stack a -> Int
height Empty = 0
height (Top valor pilha) = 1 + (height pilha)

top :: Stack a -> Maybe a
top Empty = Nothing
top (Top valor pilha) = Just valor

isEmpty :: Stack a -> Bool
isEmpty pilha = (height pilha) == 0