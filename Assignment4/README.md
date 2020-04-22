# Testing the CPP Interpreter in Haskell


To compile run `stack build` and to run all tests do 

    stack test, run
    
To test a particular `program.cc` do

    stack build
    stack exec CPPInterpreter-exe ./test/good/program.cc
    
For more in why this is important see [here](https://github.com/ChapmanCPSC/compiler-assignments/blob/master/Interpreter/README.md).
