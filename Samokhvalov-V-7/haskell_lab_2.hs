import Students

qsort []	= []
qsort (x:xs) = qsort small ++ mid ++ qsort large
	where
	  small = [y | y<-xs, y<x]
	  mid   = [y | y<-xs, y==x] ++ [x]
	  large = [y | y<-xs, y>x]

main = do
	let mystudents = students ++ [student_pupkin]
	mapM_ putStrLn $ map (\(pos, st) -> show pos ++ ": " ++ surname st) $ zip [1..] $ qsort mystudents

