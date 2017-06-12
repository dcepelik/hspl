module Interpreter (reach, Subst, showSubst, showSubst', vars, ExecState(ExecState)) where

import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf
import Control.Monad.State

import Parser

{- data structures -}

data ExecState = ExecState [Rule] Int
instance Show ExecState where
    show (ExecState _ counter) = "ExecState(counter=" ++ show counter ++ ")"

type Subst = Maybe [(Term, Term)]
type Renames = [(String, String)]

emptySubst :: Subst
emptySubst = Just []

trace' :: String -> a -> a  
--trace' s x = trace s x
trace' s x = x

showSubst (Just s) = intercalate ", " [ (show a) ++ "/" ++ (show b) | (a, b) <- s ]
showSubst Nothing = "{no unification}"

-- Find MGU of two terms
mgu' :: Term -> Term -> Subst
mgu' (Atom a) (Atom b) = if a == b then Just [] else Nothing
mgu' (Number m) (Number n) = if m == n then Just [] else Nothing
mgu' (Variable x) t = Just [ (Variable x, t) ]
mgu' t (Variable x) = Just [ (Variable x, t) ]
mgu' (Compound f args1) (Compound g args2) =
    if f == g then mguAll args1 args2
    else Nothing
mgu' _ _ = Nothing

-- Find MGU of two terms (traced)
mgu :: Term -> Term -> Subst
mgu t1 t2 = trace' (printf "%% MGU `%s` and `%s`: %s" (show t1) (show t2) (showSubst s)) s
            where s = mgu' t1 t2

-- Find MGU of several terms
mguAll :: [Term] -> [Term] -> Subst
mguAll (t:ts) (u:us) = joinSubst s (mguAll (substAll' ts s) (substAll' us s)) where s = mgu t u
mguAll [] [] = Just []
mguAll _ [] = Nothing
mguAll [] _ = Nothing

mguGoalAndRule' :: Term -> Rule -> (Subst, [Term])
mguGoalAndRule' goal (Rule head body) = (substitution, substAll' body substitution)
    where substitution = mgu goal head

mguGoalAndRule :: Term -> Rule -> State ExecState (Subst, [Term])
mguGoalAndRule goal rule = state $ \ es -> (mguGoalAndRule' goal rule, es)

-- Produce one substitution given two
joinSubst :: Subst -> Subst -> Subst
joinSubst _ Nothing = Nothing
joinSubst Nothing _ = Nothing
joinSubst (Just s) (Just t) = Just $ [(l, r) | (l, r) <- s ] ++ t

-- Given several terms and a substitution, apply the substitution to all of them
substAll' :: [Term] -> Subst -> [Term]
substAll' [] _ = []
substAll' ts s = map (`subst'` s) ts

-- Apply substitution to a term
subst'' :: Term -> Subst -> Term
subst'' t Nothing = t
subst'' t (Just []) = t
subst'' (Variable x) (Just ((Variable x', y):ss')) = if x == x' then y else subst'' (Variable x) (Just ss')
subst'' (Compound f a) ss = Compound f (substAll' a ss)
subst'' t _ = t

subst' :: Term -> Subst -> Term
subst' t s = if t == t' then t
             else subst' t' s
             where t' = subst'' t s

subst :: Term -> Subst -> State ExecState Term
subst term subst = state $ \ es -> (subst' term subst, es)

substAll :: [Term] -> Subst -> State ExecState [Term]
substAll terms st = state $ \ es -> (substAll' terms st, es)

-- Return names of variables in a term
vars :: Term -> [String]
vars (Variable x) = [x]
vars (Compound f args) = (varsAll args)
vars _ = []

-- Return the union of all variable names in all given terms
varsAll :: [Term] -> [String]
varsAll [] = []
varsAll (t:ts) = union (vars t) (varsAll ts)

renameRule :: Rule -> State ExecState Rule
renameRule rule@(Rule head body) = do
    rs <- renames rule
    headS <- subst head rs
    bodyS <- substAll body rs
    return $ Rule headS bodyS

renames :: Rule -> State ExecState Subst
renames (Rule head body) = renames' (head:body)

renames' :: [Term] -> State ExecState Subst
renames' terms = renames'' (varsAll terms)

renames'' :: [String] -> State ExecState Subst
renames'' vars = Just <$> mapM (varRenameBit) vars

varRenameBit :: String -> State ExecState (Term, Term)
varRenameBit str = state $ \(ExecState rules counter)
    -> ((Variable str, Variable (str ++ "_" ++ (show counter))), (ExecState rules (counter + 1)))

renameAllRules :: [Rule] -> State ExecState [Rule]
renameAllRules rules = mapM (renameRule) rules

unifications :: Term -> [Rule] -> State ExecState [(Subst, [Term])]
unifications goal rules = mapM (mguGoalAndRule goal) rules

reach'' :: [Term] -> [Rule] -> Subst -> State ExecState [Subst]
reach'' [] _ substitution = state $ \ es -> ([substitution], es)
reach'' (g:gs) program substitution = do
    renamedRules <- renameAllRules program
    unifs <- unifications g renamedRules
    newSubsts <- mapM id [ (reach' (b ++ (substAll' gs s)) program (joinSubst substitution s)) | (s, b) <- filter (isJust . fst) unifs ]
    return $ concat newSubsts


{- traced version of reach'' -}
reach' gs prog s = trace' (printf "goals: %s" (intercalate ", " $ map show gs)) $ reach'' gs prog s

reach :: Term -> [Rule] -> State ExecState [Subst]
reach goal program = reach' [goal] program emptySubst

showSubst' :: Subst -> [String] -> String
showSubst' Nothing _ = "false."
showSubst' _ [] = "true."
showSubst' subst vars = intercalate ", " $ map (\ var -> var ++ "/" ++ (show (showSubstVar subst (Variable var)))) vars

showSubstVar :: Subst -> Term -> Term
showSubstVar subst term = if newTerm == term then term
                          else showSubstVar subst newTerm
                          where newTerm = subst' term subst
