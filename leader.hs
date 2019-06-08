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
import Data.List

{--
    Calculating the results

    Inputs: the points list and the limit distance

    Outputs: the sum of euclidian distances and the list of groups
--}
calculateResults :: [[Double]] -> Double -> (Double, [[Int]])
calculateResults points limit = (calculateSSE groups, indeces)
    where
        groups  = calculateGroups points limit
        indeces = pointsToIndeces points groups

{--
    Calculating the sum of euclidian distances

    Inputs: the points list and the groups list

    Output: the sum of euclidian distances
--}
calculateSSE :: [[[Double]]] -> Double
calculateSSE groups =
    sum [sum [dist (groups!!i!!j) i | j <- [0 .. length (groups!!i) - 1]]
    | i <- [0 .. length groups - 1]]
    where
        center index = centerOfMass (groups !! index)
        dist point index = (pointDistance point (center index)) ** 2

{--
    Calculating the groups

    Inputs: the points list and the limit distance

    Output: the list of groups
--}
calculateGroups :: [[Double]] -> Double -> [[[Double]]]
calculateGroups [] _ = []
calculateGroups (point:points) limit =
    group : calculateGroups [p | p <- points, (pointDistance p point) > limit] limit
    where
        group = point : [p | p <- points, (pointDistance p point) <= limit]

{--
    Creating the groups recursivelly

    Inputs: the list of points, the limit distance, the index that will be analyzed and the 
--}

{--
    Calculating the center of mass of a giving group

    Inputs: the list of points, separated in the respective groups

    Output: the group's center of mass
--}
centerOfMass :: [[Double]] -> [Double]
centerOfMass [] = []
centerOfMass [x] = x
centerOfMass group = [i / toDouble (length group) | i <- pointsSummed]
    where
        pointsSummed = pointSum group
        toDouble = fromInteger . toInteger

{--
    Gathering the points with the groups list

    Inputs: the list of points and the list of groups

    Output: the list of groups, where in each position that had an index now has a point
--}
groupPoints :: [[Double]] -> [[Int]] -> [[[Double]]]
groupPoints _ [] = []
groupPoints points (g:groups) = [firstGroup] ++ groupPoints points groups
    where
        firstGroup = [points !! (index - 1) | index <- g]

{--
    Converting a grouped points list into a grouped indeces list

    Inputs: the original list of points and the list of grouped points

    Output: the list of grouped indeces
--}
pointsToIndeces :: [[Double]] -> [[[Double]]] -> [[Int]]
pointsToIndeces _ [] = []
pointsToIndeces points grouped = indeces : pointsToIndeces points (tail grouped)
    where
        group = head grouped
        indeces = [(unmaybeNum $ elemIndex point points) + 1 | point <- group]

-- Auxiliar function to "unmaybe" numbers
-- It's used in a context where m is never equal to Nothing
unmaybeNum :: Maybe Int -> Int
unmaybeNum m =
    case m of
        Nothing -> -1
        Just x -> x
