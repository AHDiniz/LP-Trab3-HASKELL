{--
	Programming Languages - Assignement #3

	Alan Herculano Diniz

	Grouping problem solution with the leader algorithm

	main.hs: program's entry point
--}

-- Step 1: make the algorithm without I/O
-- Step 2: implement command line I/O to the program
-- Step 3: implement file I/O to the program

{--
	Calculating the results

	Inputs: the points list and the limit distance

	Outputs: the sum of euclidian distances and the list of groups
--}
calculateResults :: [[Float]] -> Float -> (Float, [[Integer]])
calculateResults points limit = undefined

{--
	Calculating the sum of euclidian distances

	Inputs: the points list and the groups list

	Output: the sum of euclidian distances
--}
calculateSSE :: [[Float]] -> [[Integer]] -> Float
calculateSSE points groups = undefined

{--
	Calculating the groups

	Inputs: the points list and the limit distance

	Output: the list of groups
--}
calculateGroups :: [[Float]] -> Float -> [Int]
calculateGroups points limit = undefined

{--
	Calculating the center of mass of a giving group

	Inputs: the points list and the group

	Output: the group's center of mass
--}
centerOfMass :: [[Float]] -> [[Integer]] -> [Float]
centerOfMass points groups = undefined

{--
	Calculating the distance between two points

	Inputs: the points that will be operated over

	Output: the distance between the given points
--}
pointDistance :: [Float] -> [Float] -> Float
pointDistance a b = sqrt $ sum [(a!!i - b!!i) * (a!!i - b!!i) | i <- [0 .. length a - 1]]
