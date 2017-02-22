import Data.List

data Term = Atom String
          | Number Int
          | Variable String
          | Compound String [Term]

instance Show Term where
    show (Atom a) = a
    show (Number n) = show n
    show (Variable x) = x
    show (Compound f args) = f ++ "(" ++ (intercalate ", " [ show arg | arg <- args]) ++ ")"

data Rule = Rule Term [Term]
          deriving Show

type Subst = [(Term, Term)]

showSubst s = intercalate "   " [ (show a) ++ "/" ++ (show b) | (a, b) <- s ]

program :: [Rule]
program = [Rule (Compound "male" [Atom "david"]) []]

goal :: Term
goal = Compound "male" [Variable "X"]

subst :: Term -> Subst -> Term
subst t [] = t
subst (Variable x) ((Variable x', y):ss') = if x == x' then y else subst (Variable x) ss'
subst (Compound f a) ss = Compound f (substList a ss)
subst t _ = t

substList :: [Term] -> Subst -> [Term]
substList [] _ = []
substList (t:ts) ss = (subst t ss):(substList ts ss)

mguList :: [Term] -> [Term] -> Subst
mguList ts us = case (ts, us) of
    ([], []) -> []
    (t:ts', u:us') -> (mgu t u) ++ (mguList (substList ts' (mgu t u)) (substList us' (mgu t u)))

mgu :: Term -> Term -> Subst
mgu t1 t2 = case (t1, t2) of
    (Atom a, Atom b) -> []
    (Variable x, _) -> [(Variable x, t2)]
    (_, Variable x) -> [(Variable x, t1)]
    (Compound f1 a1, Compound f2 a2) -> mguList a1 a2

getVars :: Term -> [String]
getVars (Variable x) = [x]
getVars (Compound f args) = concat [ getVars arg | arg <- args ]
getVars _ = []

renameVar :: String -> String -> Term -> Term
renameVar oldName newName (Variable x) = if x == oldName then Variable newName
                                         else Variable x
renameVar oldName newName (Compound f args) = Compound f [ renameVar oldName newName arg | arg <- args ]
renameVar _ _ u = u

renameVars' :: [String] -> Term -> Term
renameVars' [] u = u
renameVars' (var:rest) u = renameVars' rest (renameVar var (var ++ "'") u)

renameVars :: Term -> Term -> Term
renameVars t u = renameVars' (getVars u) t

unify :: Term -> Rule -> [Term]
unify term (Rule head body) = substList body (mgu term (renameVars head term))
