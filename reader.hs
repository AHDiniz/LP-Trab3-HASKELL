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

readDouble :: String -> Double
readDouble = read

parseInput :: String -> String -> ([[Double]], Double)
parseInput pointsStr distStr = (points, limit)
    where
        treated = map words $ lines pointsStr
        points = [[readDouble j | j <- i] | i <- treated]
        limit = read distStr :: Double

createOutput :: (Double, [[Int]]) -> (String, String)
createOutput (sse, groups) = (show sse ++ "\n", groupsStr)
    where
        groupsStr = unlines [unwords [show (groups!!i!!j) | j <- [0 .. length (groups!!i) - 1]] | i <- [0 .. length groups - 1]]
