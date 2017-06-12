import Control.Monad.State
import Interpreter
import Parser
import System.IO

main :: IO ()
main = do
    code <- readFile "/home/david/sw/prolog/tests/peano.pl"
    let clauses = parse program code
    putStr $ concat (map (\ c -> "% " ++ (show c) ++ "\n") clauses)
    putStrLn "%%"

    putStr "?- "
    hFlush stdout
    input <- getLine
    let goal = parse term input

    let (substs, newState) = runState (reach goal clauses) (ExecState clauses 0)

    putStrLn $ concat $ map (\ s -> ((showSubst' s (vars goal)) ++ "\n")) $ take 20 substs

    putStrLn ""
