# CPSC-402 Assignment 3 (Type checker for C++)
## Chase Toyofuku-Souza, Corey McCrea, Ryan Kassab, Eric Lim, Michelle Kutsanov
## About
A typechecker for a fragment of the C++ programming language. The type checker returns an "OK" at success, and reports a type error at failure. It is implemented in Haskell. 

### Files
- TypeChecker.hs

### Submission 1 Problems
- The provided template passes `do_nothing.cc` and `void_return_empty.cc`
- The typechecking rules already implemented are SExp, SDecls, SReturn, EInt, ETimes, EAss and ETyped. 
- The rule that needs to be added to typecheck `easy_add.cc` is EPlus, under inferTypeExp.
- In order to to typecheck the program `ass_easy.cc`, we must add the rules EId and SInit. SInit is used to initialize an Id with the value of an Exp. EId is used to infer the type of the Id, so that SInit will properly evaluate if the types of both sides are matching.
