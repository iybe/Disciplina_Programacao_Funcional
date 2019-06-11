data Day = Day Int deriving (Eq,Ord,Show)
data Month = Janeiro | Fevereiro | Março | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving (Eq,Ord,Show,Enum,Bounded)
data Year = Year Int deriving (Eq,Ord,Show)
data Date = Date {ano :: Year , mes :: Month , dia :: Day} deriving (Eq,Ord,Show)

sortListDates :: [Date] -> [Date]
sortListDates [] = []
sortListDates (x:xs) = ladoE++[x]++ladoD
    where ladoE = sortListDates [a | a <- xs, a <= x]
          ladoD = sortListDates [a | a <- xs, a > x]