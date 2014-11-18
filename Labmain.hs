module Main where
import Students

qsortStudent :: [Student] -> [Student]
qsortStudent [] = []
qsortStudent (x:xs) = qsortStudent min ++ [x] ++ qsortStudent max
    where
        min  = [ y | y <- xs, iq y < iq x ]
        max = [ y | y <- xs, iq y >= iq x ]

iqStudents = qsortStudent students

main = do
putStrLn "Students list, acording to iq:"
mapM_ putStrLn $ map (\(pos, st) -> show pos ++ ") " ++ (name st) ++ " " ++ (surname st) ++ " " ++ show (iq st)) $ zip [1..] iqStudents
