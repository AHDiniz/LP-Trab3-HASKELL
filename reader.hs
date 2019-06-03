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

parseInput :: String -> String -> ([[Float]], Float)
parseInput pointsStr distStr = ([[0.0, 1.1]], 0)

createOutput :: (Float, [[Int]]) -> (String, String)
createOutput (sse, groups) = (show sse, show groups)
