{--
	Programming Languages - Assignement #3

	Alan Herculano Diniz

	Grouping problem solution with the leader algorithm

	leader.hs: leader algorithm implementation
--}

-- Step 1: make the algorithm without I/O
-- Step 2: implement command line I/O to the program
-- Step 3: implement file I/O to the program

import Points

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
centerOfMass :: [[Float]] -> [Int] -> [Float]
centerOfMass points group = [a / fromIntegral $ length group | a <- pointSum groupPoints]
	where
		indexes = [group !! i - 1 | i <- [0 .. length group - 1]]
		groupPoints = [points !! i | i <- indexes]
