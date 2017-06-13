import Control.Monad.State
import Interpreter
import Parser
import System.IO
import System.Environment

main :: IO ()
main = do
    (filename:_) <- getArgs
    code <- readFile filename
    let prog = parse program code

    putStrLn $ "\t%% Input file is " ++ filename
    putStrLn $ "\t%% Loaded " ++ (show $ length prog) ++ " prog rules"
    putStr $ concat (map (\ c -> "\t\t%%" ++ (show c) ++ "\n") prog)
    putStrLn "\t%% Ready."
    putStrLn ""

    mainloop prog

mainloop :: Program -> IO ()
mainloop prog = do

    putStr "?- "
    hFlush stdout
    input <- getLine
    let goal = parse term input

    let (substs, _) = runState (reach goal) (ExecState prog 0)
    if substs /= [] then putStrLn $ concat $ map (\ s -> ((showSubstVars s (varNames goal)) ++ "\n")) $ substs
    else putStrLn "false." >> putStrLn ""

    mainloop prog
