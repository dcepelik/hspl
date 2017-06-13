> module Interpreter (reach, Subst, showSubstVars, varNames, ExecState(ExecState)) where

> import Control.Monad.State
> import Data.List
> import Data.Maybe
> import Debug.Trace
> import Parser
> import Text.Printf

First, we introduce ExecState. ExecState holds the program (the rules) and a counter used for
renaming of variables.

> data ExecState = ExecState [Rule] Int

Next, we introduce a substitution. A substitution is either Nothing or Just a list of pairs of
terms. The first term is always a Variable, the second is whatever should be substituted in place
of that variable.

> type Subst = Maybe [(Term, Term)]

Subst is a type and cannot be a Show instance. We therefore use showSubst to get a pretty
human-readable string representation of the substitution.

> showSubst :: Subst -> String
> showSubst (Just subst) = showSubstVars (Just subst) (varNamesMany $ map fst subst)
> showSubst Nothing = "[no subst]"

Given a substitution and a variable, substitute the variable recursively as long as the application
of the substitution changes the variable's contents. This is used byt he Main module to print the
ultimate value of free variables in the user's query.

> showSubstVar :: Subst -> Term -> Term
> showSubstVar subst term = if newTerm == term then term
>                           else showSubstVar subst newTerm
>                           where newTerm = subst' term subst

Given a substitution and a list of variable names, use showSubstVar to print the definitive values
of the variables.

> showSubstVars :: Subst -> [String] -> String
> showSubstVars Nothing _ = "false."
> showSubstVars _ [] = "true."
> showSubstVars subst varNames = intercalate ", " $ map (\ var -> var ++ " = " ++ (show (showSubstVar subst (Variable var)))) varNames

Introduce empty substitution so that we don't have to write (Just []) all the time.

> emptySubst :: Subst
> emptySubst = Just []

TODO

> trace' :: String -> a -> a  
> --trace' s x = trace s x
> trace' s x = x

Find most general unification (MGU) of two terms. This is easy in most cases, the only trickier
case is when we attempt to find MGU of two compound terms (then we have to use mguList).

> mgu' :: Term -> Term -> Subst

> mgu' (Atom a) (Atom b) = if a == b then Just [] else Nothing
> mgu' (Number m) (Number n) = if m == n then Just [] else Nothing
> mgu' (Variable x) t = Just [ (Variable x, t) ]
> mgu' t (Variable x) = Just [ (Variable x, t) ]
> mgu' (Compound f args1) (Compound g args2) =
>     if f == g then mguList args1 args2
>     else Nothing
> mgu' _ _ = Nothing

Given two lists of terms, find an MGU that unifies corresponding pairs in the lists (after
subtitution, the first, second, third, ... terms in both lists should be the same).

> mguList :: [Term] -> [Term] -> Subst
> mguList (t:ts) (u:us) = joinSubst s (mguList (substAll' ts s) (substAll' us s)) where s = mgu' t u
> mguList [] [] = Just []
> mguList _ [] = Nothing
> mguList [] _ = Nothing

TODO

> mgu :: Term -> Term -> Subst
> mgu t1 t2 = trace' (printf "%%%% MGU `%s` and `%s`: %s" (show t1) (show t2) (showSubst s)) s
>             where s = mgu' t1 t2

TODO

> mguGoalAndRule' :: Term -> Rule -> (Subst, [Term])
> mguGoalAndRule' goal (Rule head body) = (substitution, substAll' body substitution)
>     where substitution = mgu goal head

TODO

> mguGoalAndRule :: Term -> Rule -> State ExecState (Subst, [Term])
> mguGoalAndRule goal rule = state $ \ es -> (mguGoalAndRule' goal rule, es)

Merge two substitutions into one. Application of the resulting substitution should have the same
effect as sequential application of the two original substitutions (in any order).

Note that, if either substitution is Nothing, the result is Nothing as well.

> joinSubst :: Subst -> Subst -> Subst
> joinSubst _ Nothing = Nothing
> joinSubst Nothing _ = Nothing
> joinSubst (Just s) (Just t) = Just $ s ++ t

TODO

> substAll' :: [Term] -> Subst -> [Term]
> substAll' [] _ = []
> substAll' ts s = map (`subst'` s) ts

TODO

> subst'' :: Term -> Subst -> Term
> subst'' t Nothing = t
> subst'' t (Just []) = t
> subst'' (Variable x) (Just ((Variable x', y):ss')) = if x == x' then y else subst'' (Variable x) (Just ss')
> subst'' (Compound f a) ss = Compound f (substAll' a ss)
> subst'' t _ = t

> subst' :: Term -> Subst -> Term
> subst' t s = if t == t' then t
>              else subst' t' s
>              where t' = subst'' t s

> subst :: Term -> Subst -> State ExecState Term
> subst term subst = state $ \ es -> (subst' term subst, es)

> substAll :: [Term] -> Subst -> State ExecState [Term]
> substAll terms st = state $ \ es -> (substAll' terms st, es)

Return names of variables in a term.

> varNames :: Term -> [String]
> varNames (Variable x) = [x]
> varNames (Compound f args) = (varNamesMany args)
> varNames _ = []

Return the union of all variable names in all given terms

> varNamesMany :: [Term] -> [String]
> varNamesMany [] = []
> varNamesMany (t:ts) = union (varNames t) (varNamesMany ts)

Rename all variables in a rule. Assuming no variable in the program or goal uses undescore (_)
in its name, variables in the renamed rule are guaranteed not to interfere with variables in other
rules or goals.

This is very important. When we success at unifying a rule's head with our current goal,
we remove that goal off the goals list and apply the subtitution that resulted in the unification
to all remaining goals. Because variable names are guarantedd not to clash, we actually simulate
a scope for the variables: only variables that shared the same scope (rule) may be called the same.

> rename :: Rule -> State ExecState Rule
> rename rule@(Rule head body) = do
>     rs <- renameSubst rule
>     head' <- subst head rs
>     body' <- substAll body rs
>     return $ Rule head' body'

Given several rules, rename all of them. This is a stateful action.

> renameMany :: [Rule] -> State ExecState [Rule]
> renameMany rules = mapM (rename) rules

Get a substitution which, upon application, will perform the unique renaming of variables
in a rule. This is a stateful action.

> renameSubst :: Rule -> State ExecState Subst
> renameSubst (Rule head body) = renameSubst' (head:body)

TODO

> renameSubst' :: [Term] -> State ExecState Subst
> renameSubst' terms = renameSubst'' (varNamesMany terms)

TODO

> renameSubst'' :: [String] -> State ExecState Subst
> renameSubst'' varNames = Just <$> mapM (assignNewName) varNames

Given a variable name, return a new unique name for the variable. The ExecState is modified
by incrementing the rename-counter. This is a stateful action.

> assignNewName :: String -> State ExecState (Term, Term)
> assignNewName str = state $ \(ExecState rules counter)
>     -> ((Variable str, Variable (str ++ "_" ++ (show counter))), (ExecState rules (counter + 1)))

Given a goal and a set of program rules, return a list of pairs, where the first component of each
pair is the substitution that results in the MGU of the goal with a rule's head, and the second
component is the result of applying that substitution to that rule's body.

This is a stateful action.

> unifications :: Term -> [Rule] -> State ExecState [(Subst, [Term])]
> unifications goal rules = mapM (mguGoalAndRule goal) rules

> canUnify :: Term -> Term -> Bool
> canUnify t1 t2 = (mgu t1 t2) /= Nothing

This is the single most important procedure performing the resolution.

Given a set of goals, program clauses and a substitution that got us into this state of computation
so far, keep substituting until there are no more goals, then return the resulting substitution
that was accumulated along the way.
  
This is a stateful action.

> reach'' :: [Term] -> Subst -> State ExecState [Subst]
> reach'' [] s = state $ \ es -> ([s], es)
> reach'' ((Compound "ne" [t1, t2]):gs) s' =
>  	  if (not $ canUnify t1 t2) then reach'' gs s'
>  	  else return []
> reach'' (g:gs) s' = state $ \ es@(ExecState rules _) -> runState (do
>     renamedRules <- renameMany rules
>     unifs <- unifications g renamedRules
>     newSubsts <- mapM id [ (reach' (b ++ (substAll' gs s)) (joinSubst s' s)) | (s, b) <- filter (isJust . fst) unifs ]
>     return $ concat newSubsts) es

TODO

> reach' gs s = trace' (printf "%%%% goals: %s" (intercalate ", " $ map show gs)) $ reach'' gs s

This is a wrapper around reach'' to provide a nicer API to the Main module.

> reach :: Term -> State ExecState [Subst]
> reach goal = reach' [goal] emptySubst

reachNew :: Term -> State ExecState [Subst]
reachNew goal = state $ \ es@(ExecState rules _) -> runState (reach goal) es
