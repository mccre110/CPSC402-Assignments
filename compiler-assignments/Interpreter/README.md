# README.md

For the Haskell an Scala version we provide a testing environment that allows **to automatically test against all test programs**, but also to test just one particular program.

**To test and study particular programs** is important, if the interpreter does not give the right result on a particular `program.cc` (such as, for example, looping infinitely). The way forward then is to

- make  `program.cc` so small that the error does not occur anymore
- make  `program.cc` bigger so that the error comes back
- iterate the above until you found the smallest version of `program.cc` that exhibits the bug
- insert `printInt` statements as needed to track the execution of the program
- guess what could cause the problem and change `Interpreter.hs`
- iterate all of the above

The reason why debugging the interpreter is more difficult than debugging the typechecker is that the interpreter is not purely compositional in the abstract syntax. For example, even if it looks as if a wrong interpretation of `SWhile` might be resonsible for an infinite loop, the problem could also lie in any of the cases (eg `Sif`, `SDecl`, `SInit`, `EAss`, etc) as they all change the environment and therefore can change the values of the variables involved in testing the condition responsible for entering the `while`.

If all of this does not help, save this particular program for the end and try to get the interpreter first working on all of the other programs. If you are lucky, this will already solve the problem of `program.cc` as well.
