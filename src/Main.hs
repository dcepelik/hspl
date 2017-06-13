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

    putStrLn $ "Loaded " ++ (show $ length prog) ++ " prog rules from " ++ filename
    putStr $ concat (map (\ c -> "% " ++ (show c) ++ "\n") prog)
    putStrLn "%%%%"

    mainloop prog

mainloop :: Program -> IO ()
mainloop prog = do

    putStr "?- "
    hFlush stdout
    input <- getLine
    let goal = parse term input

    let (substs, _) = runState (reach goal) (ExecState prog 0)
    putStrLn $ concat $ map (\ s -> ((showSubstVars s (varNames goal)) ++ "\n")) $ substs

    mainloop prog
