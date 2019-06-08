{--
    Programming Languages - Assignement #3

    Alan Herculano Diniz

    Grouping problem solution with the leader algorithm

    main.hs: program's entry point
--}

import System.Environment
import Leader
import Reader

main :: IO()
main = do
    args <- getArgs
    pointsStr <- readFile $ head args
    distStr <- readFile $ head $ tail args
    let (points, limit) = parseInput pointsStr distStr
    let (sse, groups) = calculateResults points limit
    let (sseStr, groupsStr) = createOutput (sse, groups)
    writeFile "results.txt" groupsStr
    writeFile "saida.txt" sseStr
