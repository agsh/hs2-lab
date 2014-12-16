module TheologyStudent(thisSubject,howManySubjects,oneSubjectStudents,toNewSubject) where
import Students

thisSubject :: (String,String) -> [(String,String)] -> Integer
thisSubject fullName subjectsList 
	|(elem fullName subjectsList == True) = 1
	|otherwise = 0

howManySubjects :: [Student] -> [Student]
howManySubjects [] = []
howManySubjects (x:xs)
	| thisSubject (name x, surname x) french + thisSubject (name x, surname x) discrete_mathematics + thisSubject (name x, surname x) programming == 1 = (x : howManySubjects xs)
	| otherwise = howManySubjects xs

oneSubjectStudents :: [Student]
oneSubjectStudents = howManySubjects students

toNewSubject :: [Student] -> [(String,String)]
toNewSubject [] = []
toNewSubject (x:xs) =( (name x, surname x) : toNewSubject xs )

main = do
putStrLn "Forcibly enrolled in the course of theology:"
mapM_ putStrLn $ map (\(pos, st) -> show pos ++ ") " ++ (name st) ++ " " ++ (surname st)) $ zip [1..] oneSubjectStudents
return ()