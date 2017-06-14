# hspl

## Abstract

`hspl` is a very simple interpreter of a subset of Prolog written in Literate
Haskell. I wrote it as an exercise to grasp basics of both Prolog and Haskell.

It may be interesting for you if you want to play around with either Prolog or
Haskell, or if you're interested in some of topics below.

Feel free to contact me if you have any questions.

## Topics

  * "Nondeterministic" monadic parsers
  * Global state in Haskell using `Control.Monad.State`
  * Basic implementation of Prolog resolution method and backtracking

### Unsupported Features

  * Most of Prolog features: arithmetics, operators (such as `=`, `is`, etc.)
  * I didn't use monad transformers to intermix `State` with `IO`, so I cannot
    use ie. the `consult("filename")` predicate to load the file during
	execution

## Docs

I didn't provide any "User's Manual", as there are hardly any users to this;
it's meant as a learning aid to other novice Haskell programers.

## Tests

Sure enough, there are bugs.

Test inputs may be found in `tests/`. I didn't provide an automated test runner,
but generally whatever you find in `tests/` should work well.

In the unlikely case that you needed basic rules for list manipulation, for
example, you may find them in `tests/lists.pl`.

## Overview

The sources constitute of:

  * `Main.hs`, the application's entry point 
  * `Interpreter.lhs`, the Prolog interpreter itself, and
  * `Parser.lhs`, the monadic parser of Prolog.

## Sources

  * [My question on Code Review Stack Exchange](https://codereview.stackexchange.com/questions/123600/prolog-parser-written-in-haskell)
    regarding the `Parser` module.
