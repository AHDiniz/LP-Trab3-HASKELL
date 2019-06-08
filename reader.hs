{--
    Programming Languages - Assignement #3

    Alan Herculano Diniz

    Grouping problem solution with the leader algorithm

    reader.hs: input and output functions
--}

module Reader(
 parseInput,
 createOutput
) where

import Text.Printf

-- Parsing a string to a double
readDouble :: String -> Double
readDouble = read

{--
    Getting the input files string and parsing them to the proper data types

    Inputs: the string with the points and the distance

    Output: tuple with the points list and the limit distance
--}
parseInput :: String -> String -> ([[Double]], Double)
parseInput pointsStr distStr = (points, limit)
    where
        treated = map words $ lines pointsStr
        points = [[readDouble j | j <- i] | i <- treated]
        limit = readDouble distStr

{--
    Creating the output string with the results of the grouping algorithm

    Inputs: the sse and the groups list

    Output: tuple with the sse file string and the groups file string
--}
createOutput :: Double -> [[Int]] -> (String, String)
createOutput sse groups = (printf "%.4f\n" sse, groupsStr)
    where
        groupsStr = unlines [unwords [show (groups !! i !! j) | j <- [0 .. length (groups!!i) - 1]] | i <- [0 .. length groups - 1]]
