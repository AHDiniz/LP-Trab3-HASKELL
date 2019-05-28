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
calculateResults :: [[Float]] -> Float -> (Float, [[Int]])
calculateResults points limit = (calculateSSE points groups, groups)
    where
        groups = calculateGroups points limit

{--
    Calculating the sum of euclidian distances

    Inputs: the points list and the groups list

    Output: the sum of euclidian distances
--}
calculateSSE :: [[Float]] -> [[Int]] -> Float
calculateSSE points groups = sum [sum [pointDistance (groupedPoints !! i !! j) (center i) ** 2 | j <- [0 .. length (groupedPoints !! i) - 1]] | i <- [0 .. length groupedPoints - 1]]
    where
        groupedPoints = groupPoints points groups
        center index = centerOfMass (groupedPoints !! index)

{--
    Calculating the groups

    Inputs: the points list and the limit distance

    Output: the list of groups
--}
calculateGroups :: [[Float]] -> Float -> [[Int]]
calculateGroups points limit = createGroups points limit 0 []

{--
    Auxiliar function that creates the groups list given it's current state

    Inputs: the list of points, the limit distance, the current index that'd been analyzed
    and the list of groups that's in construction

    Output: the completed list of groups
--}
createGroups :: [[Float]] -> Float -> Int -> [[Int]] -> [[Int]]
createGroups points limit index [] = [[index + 1]] ++ createGroups points limit (index + 1) [[index + 1]] -- First recursion's "iteration"
createGroups [] _ _ groups = groups -- Recursion's end condition
createGroups points limit index groups = newGroups ++ createGroups (tail points) limit (index + 1) newGroups -- General case recursion
    where
        group = last groups -- Current group that is been analyzed
        leader = points !! ((group !! 0) - 1) -- Getting the current group leader
        point = points !! index -- Current point that's being analysed
        dist = pointDistance leader point -- Calculating the distance between the leader and the current point
        newGroups = if dist <= limit then init groups ++ [group ++ [index + 1]] else groups ++ [[index + 1]] -- New groups list

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
        toFloat x = fromInteger $ toInteger x

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
