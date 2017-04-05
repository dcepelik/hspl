import Parser
import Interpreter
import System.IO

main :: IO ()
main = do
    programText <- readFile "/home/david/sw/prolog/tests/peano.pl"
    let clauses = parse program programText
    putStr $ concat (map (\ c -> "% " ++ (show c) ++ "\n") clauses)
    putStrLn "%%"

    putStr "?- "
    hFlush stdout
    input <- getLine
    let goal = parse term input

    putStrLn $ concat $ map (\ s -> ((showSubst s) ++ "\n")) $ take 10 (reach clauses goal)

    putStrLn ""
