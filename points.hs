{--
    Programming Languages - Assignement #3

    Alan Herculano Diniz

    Grouping problem solution with the leader algorithm

    points.hs: point manipulation function library
--}

module Points(
 pointDistance,
 pointSum
) where

{--
    Calculating the distance between two points

    Inputs: the points that will be operated over

    Output: the distance between the given points
--}
pointDistance :: [Float] -> [Float] -> Float
pointDistance a b = sqrt $ sum [(a !! i - b !! i) * (a !! i - b !! i) | i <- [0 .. length a - 1]]

{--
    Adding multiple points

    Input: the point list

    Output: the sum of the points
--}
pointSum :: [[Float]] -> [Float]
pointSum [a, b] = [a !! i + b !! i | i <- [0 .. length a - 1]]
pointSum points = pointSum [head points, pointSum $ tail points]
