module Interpreter (reach, Subst) where

import Data.List
import Data.Maybe

import Parser

{- data structures -}

type Subst = Maybe [(Term, Term)]

showSubst (Just s) = intercalate ", " [ (show a) ++ "/" ++ (show b) | (a, b) <- s ]

mgu :: Term -> Term -> Subst
mgu (Atom a) (Atom b) = if a == b then Just [] else Nothing
mgu (Number m) (Number n) = if m == n then Just [] else Nothing
mgu (Variable x) t = Just [ (Variable x, t) ]
mgu t (Variable x) = Just [ (Variable x, t) ]
mgu (Compound f args1) (Compound g args2) =
    if f == g then mguAll args1 args2
    else Nothing

mguAll :: [Term] -> [Term] -> Subst
mguAll (t:ts) (u:us) = joinSubst s (mguAll (substAll ts s) (substAll us s)) where s = mgu t u
mguAll [] [] = Just []
mguAll _ [] = Nothing
mguAll [] _ = Nothing

joinSubst :: Subst -> Subst -> Subst
joinSubst _ Nothing = Nothing
joinSubst Nothing _ = Nothing
joinSubst (Just s) (Just t) = Just $ s ++ t

substAll :: [Term] -> Subst -> [Term]
substAll [] _ = []
substAll ts s = map (`subst` s) ts

subst :: Term -> Subst -> Term
subst t Nothing = t
subst t (Just []) = t
subst (Variable x) (Just ((Variable x', y):ss')) = if x == x' then y else subst (Variable x) (Just ss')
subst (Compound f a) ss = Compound f (substAll a ss)
subst t _ = t

vars :: Term -> [String]
vars (Variable x) = [x]
vars (Compound f args) = concat [ vars arg | arg <- args ]
vars _ = []

renameVar :: String -> String -> Term -> Term
renameVar oldName newName (Compound f args) = Compound f [ renameVar oldName newName arg | arg <- args ]
renameVar oldName newName (Variable x) = if x == oldName then Variable newName else Variable x
renameVar _ _ u = u

renameAllVars :: [String] -> Term -> Term
renameAllVars [] u = u
renameAllVars (var:rest) u = renameAllVars rest (renameVar var (var ++ "'") u)

renameVars :: Term -> Term -> Term
renameVars t u = renameAllVars (vars u) t

unify :: Term -> Rule -> [Term]
unify term (Rule head body) = substAll body (mgu term (renameVars head term))

reach' :: Program -> [Term] -> Subst -> [Subst]
reach' _ [] s = [s]
reach' program (g:gs) s = concat [ (reach' program (substAll (b ++ gs) s') (joinSubst s s')) | (s', b) <- [ (mgu g (renameVars h g), b) | (Rule h b) <- program ], isJust s' ]

reach :: Program -> Term -> [Subst]
reach program goal = reach' program [goal] (Just [])
