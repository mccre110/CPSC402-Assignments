# CPPTypeChecker

A type checker for a subset of C++ written in Haskell.

Set up and written by Samuel Balco following Chapter 4 of [Implementing Programming Languages](http://www.grammaticalframework.org/ipl-book/) by Aarne Ranta and the corresponding [Assignment 2](http://www.grammaticalframework.org/ipl-book/assignments/assignment2/assignment2.html).

To compile run `stack build` and to test run `stack test`.

To run typechecking on a specific file, run `stack exec CPPTypeChecker-exe <file_path>.cc`.

The files produced by bnfc are in src:

	AbsCpp.hs
	ErrM.hs
	LexCpp.hs
	ParCpp.hs
	PrintCpp.hs

The template for the typechecker is in `TypeChecker.hs`. This where you will find the code that you need to complete.

To know which cases you need to add, look at the grammar as well as at the algebraic data type for abstract syntax trees defined in `AbsCpp.hs`.

The error monad that is used to modify the type `Type` of CPP-types is defined in `ErrM.hs`.


I keep an updating [list with tips](https://hackmd.io/nVQP-fp-TEWUbp9kecaLTQ).

