--HW2

repeatedSeq n = [x | y <- [1..n], x <- [1..y]]

grade [] = []
grade (x:xs) | x >= 90 && x <= 100 = ['A'] ++ grade xs
             | x >= 80 && x <= 89  = ['B'] ++ grade xs
             | x >= 70 && x <= 79  = ['C'] ++ grade xs
             | x >= 60 && x <= 69  = ['D'] ++ grade xs
             | x < 60  = ['F'] ++ grade xs

fibb n | n == 0 = []
       | n == 1 = [1] ++ fibb (n - 1)
       | n == 2 = [1] ++ fibb (n - 1)
       | n > 2  = [head (fibb (n - 1)) + head (fibb (n - 2))] ++ fibb (n - 1)

allOcrDelete _ [] = []
allOcrDelete n (x:xs) | x == n = [] ++ allOcrDelete n xs
                      | otherwise = [x] ++ allOcrDelete n xs

coPrime m n = if(length xs < 2)
           then True
           else
           False
           where
           xs = [x | x <- [1..(m + n)], (m `mod` x) == 0, (n `mod` x) == 0]
