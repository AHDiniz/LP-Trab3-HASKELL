{--
    Programming Languages - Assignement #3

    Alan Herculano Diniz

    Grouping problem solution with the leader algorithm

    leader.hs: leader algorithm implementation
--}

-- Step 1: make the algorithm without I/O
-- Step 2: implement command line I/O to the program
-- Step 3: implement file I/O to the program

module Leader(
 calculateResults
) where

import Points

{--
    Calculating the results

    Inputs: the points list and the limit distance

    Outputs: the sum of euclidian distances and the list of groups
--}
calculateResults :: [[Float]] -> Float -> (Float, [[Int]])
calculateResults points limit = (calculateSSE groups, indeces)
    where
        groups  = calculateGroups points limit
        indeces = [[i + j | j <- [0 .. length (groups !! i) - 1]] | i <- [0 .. length groups - 1]]

{--
    Calculating the sum of euclidian distances

    Inputs: the points list and the groups list

    Output: the sum of euclidian distances
--}
calculateSSE :: [[[Float]]] -> Float
calculateSSE groups = sum [sum [pointDistance (groups !! i !! j) (center i) ** 2 | j <- [0 .. length (groups !! i) - 1]] | i <- [0 .. length groups - 1]]
    where
        center index = centerOfMass (groups !! index)

{--
    Calculating the groups

    Inputs: the points list and the limit distance

    Output: the list of groups
--}
calculateGroups :: [[Float]] -> Float -> [[[Float]]]
calculateGroups (point:points) limit = group : calculateGroups remainder limit
    where
        distance p = (pointDistance p point)
        remainder = [p | p <- points, (distance p) > limit]
        group = point : [p | p <- points, (distance p) <= limit]

{--
    Creating the groups recursivelly

    Inputs: the list of points, the limit distance, the index that will be analyzed and the 
--}

{--
    Calculating the center of mass of a giving group

    Inputs: the list of points, separated in the respective groups

    Output: the group's center of mass
--}
centerOfMass :: [[Float]] -> [Float]
centerOfMass [] = []
centerOfMass group = [i / toFloat (length group) | i <- pointsSummed]
    where
        pointsSummed = pointSum group
        toFloat = fromInteger . toInteger

{--
    Gathering the points with the groups list

    Inputs: the list of points and the list of groups

    Output: the list of groups, where in each position that had an index now has a point
--}
groupPoints :: [[Float]] -> [[Int]] -> [[[Float]]]
groupPoints _ [] = []
groupPoints points (g:groups) = [firstGroup] ++ groupPoints points groups
    where
        firstGroup = [points !! (index - 1) | index <- g]
