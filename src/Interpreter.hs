module Interpreter (reach, Subst, showSubst, showSubst', vars) where

import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf

import Parser

{- data structures -}

type Subst = Maybe [(Term, Term)]
type Renames = [(String, String)]

trace' :: String -> a -> a  
trace' s x = trace s x

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
mguAll (t:ts) (u:us) = joinSubst s (mguAll (substAll ts s) (substAll us s)) where s = mgu t u
mguAll [] [] = Just []
mguAll _ [] = Nothing
mguAll [] _ = Nothing

-- Produce one substitution given two
joinSubst :: Subst -> Subst -> Subst
joinSubst _ Nothing = Nothing
joinSubst Nothing _ = Nothing
joinSubst (Just s) (Just t) = Just $ [(l, r) | (l, r) <- s ] ++ t

-- Given several terms and a substitution, apply the substitution to all of them
substAll :: [Term] -> Subst -> [Term]
substAll [] _ = []
substAll ts s = map (`subst` s) ts

-- Apply substitution to a term
subst :: Term -> Subst -> Term
subst t Nothing = t
subst t (Just []) = t
subst (Variable x) (Just ((Variable x', y):ss')) = if x == x' then y else subst (Variable x) (Just ss')
subst (Compound f a) ss = Compound f (substAll a ss)
subst t _ = t

-- Return variables in a term
vars :: Term -> [String]
vars (Variable x) = [x]
vars (Compound f args) = concat [ vars arg | arg <- args ]
vars _ = []

-- Does a term contain a variable?
containsVar :: Term -> String -> Bool
containsVar (Variable x') x = x == x'
containsVar (Atom a) _ = False
containsVar (Number n) _ = False
containsVar (Compound f args) x = any (`containsVar` x) args

-- Returns a new name for a variable so that it does not collide with other vars
-- TODO
newVarName :: String -> Term -> String
newVarName var term = if containsVar term varPrime then newVarName varPrime term
                      else varPrime
                      where varPrime = (var ++ "'")

-- Rename a variable in a term
renameVar :: String -> String -> Term -> Term
renameVar oldName newName (Compound f args) = Compound f [ renameVar oldName newName arg | arg <- args ]
renameVar oldName newName (Variable x) = if x == oldName then Variable newName else Variable x
renameVar _ _ u = u

-- Rename all variables in a term
renameAllVars :: [String] -> Term -> Term
renameAllVars [] u = u
renameAllVars (var:rest) u = renameAllVars rest (renameVar var (newVarName var u) u)

-- Given two terms, rename variables in the former so they do not collide with vars in the latter
renameVars :: Term -> Term -> Term
renameVars t u = renameAllVars (vars u) t

renames' :: [String] -> Term -> [(String, String)]
renames' [] _ = []
renames' (v:vs) term = [(v, v')] ++ renames' vs (renameVar v v' term) where v' = newVarName v term

renames :: Term -> Term -> Renames
renames t1 t2 = renames' (vars t1) t2

applyRenames' :: Term -> Renames -> Term
applyRenames' t [] = t
applyRenames' t ((old, new):rs) = applyRenames' (renameVar old new t) rs

applyRenames :: Rule -> Renames -> Rule
applyRenames (Rule head body) rs = Rule (applyRenames' head rs) (map (`applyRenames'` rs) body)

ruleHead :: Rule -> Term
ruleHead (Rule head _) = head

rename :: Term -> Rule -> Rule
rename term rule = applyRenames rule (renames term (ruleHead rule))

unifications :: Program -> Term -> [(Subst, [Term])]
unifications cs g = filter (isJust . fst) [ (mgu g h, b) | (Rule h b) <- map (rename g) cs ]

reach'' :: Program -> [Term] -> Subst -> [Subst]
reach'' cs [] s = [s]
reach'' cs (g:gs) s = concat [ (reach' cs ((substAll b s') ++ gs) (joinSubst s s')) | (s', b) <- unifications cs g ]

reach' :: Program -> [Term] -> Subst -> [Subst]
reach' cs gs s = trace' (printf "goals: %s" (intercalate ", " $ map show gs)) $ reach'' cs gs s

reach :: Program -> Term -> [Subst]
reach cs g = reach' cs [g] (Just [])

showSubst' :: Subst -> [String] -> String
showSubst' (Just []) vs = ""
showSubst' (Just ((Variable x, t):s)) vs = if any (== x) vs then (x ++ " = " ++ (show t) ++ ", ") ++ (showSubst' (Just s) vs)
                                           else showSubst' (Just s) vs
