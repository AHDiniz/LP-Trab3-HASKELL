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
    args <- getArgs -- Getting the command line arguments
    pointsStr <- readFile $ head args -- Opening and reading the points file
    distStr <- readFile $ head $ tail args -- Opening and reading the distance file
    let (points, limit) = parseInput pointsStr distStr -- Parsing the strings to get the data
    let (sse, groups) = calculateResults points limit -- Executing the grouping algorithm
    let (sseStr, groupsStr) = createOutput sse groups -- Creating the output strings
    writeFile "saida.txt" groupsStr -- Points output file
    writeFile "result.txt" sseStr -- Distance output file
