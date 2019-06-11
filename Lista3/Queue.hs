data Queue a = Empty | Start a (Queue a) deriving (Show)

startQueue :: Queue a -> Maybe a
startQueue Empty = Nothing
startQueue (Start valor pilha) = Just valor

endQueue :: Queue a -> Maybe a
endQueue Empty = Nothing
endQueue (Start valor Empty) = Just valor
endQueue (Start valor fila) = (endQueue fila)

pushQueue :: a -> Queue a -> Queue a
pushQueue valor Empty = Start valor Empty
pushQueue valor (Start inicio fila) = Start inicio (pushQueue valor fila)

popQueue :: Queue a -> Queue a
popQueue Empty = Empty
popQueue (Start valor fila) = fila

isEmptyQueue :: Queue a -> Bool
isEmptyQueue Empty = True
isEmptyQueue _ = False

lenQueue :: Queue a -> Int
lenQueue Empty = 0
lenQueue (Start valor fila) = 1 + (lenQueue fila)

whileNotEmpty :: (a -> b) -> Queue a -> [b]
whileNotEmpty _ Empty = []
whileNotEmpty f (Start valor fila) = [f valor]++(whileNotEmpty f fila)