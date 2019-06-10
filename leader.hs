{--
    Programming Languages - Assignement #3

    Alan Herculano Diniz

    Grouping problem solution with the leader algorithm

    leader.hs: leader algorithm implementation
--}

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
calculateSSE groups = sum [groupSum group | group <- groups]

-- SSE calculation auxiliar function
groupSum :: [[Double]] -> Double
groupSum group = sum [(pointDistance point center) ** 2| point <- group]
    where
        center = centerOfMass group

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
