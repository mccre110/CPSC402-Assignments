# compiler-assignments

Clone this repository into the repo that you use for the solutions of the assignments for the course on compiler construction.

## Assignment Typechecker 

**first deadline April 13, second deadline April 20**

The assignment is detailed [here](http://www.grammaticalframework.org/ipl-book/assignments/assignment2/assignment2.html).


### Introductory Remarks

To implement the typechecker I suggest that you use  Haskell. Java does not have algebraic data types or pattern matching.

To get you started Samuel Balco implemented the type checker in Haskell and Scala  and we can offer you template files in which you fill in the missing cases in the typechecker template file provided. 

- Haskell has the advantage that the code is the closest to the specification. One reason for this is that Haskell supports algebraic data types and pattern matching. A disadvantage of Haskell is that it requires some programming language concepts that you are likely to not have seen before, such as monads which are needed for error handling. But they are covered by what is in the template

- Scala has the advantage that it is close to Java but also incorporates features from functional programming similar to Haskell. In particular Scala has algebraic data types and pattern matching. We provide two implementations of the typechecker. Typechecker.scala was obtained by translating (by hand, not automatically) the Haskell typechecker into the object oriented language Scala. TypecheckerAlt.scala was obtained as a further modification that uses a method of error handling similar to the one of Java.

<!--
- Java has the advantage that it is the most familiar language to you. It also will give you the full experience as you are asked to produce the Java files you need using bnfc. If you have some previous experience with Java you should be able to see how to proceed from there by looking at Chapter 4 of the book and the Scala template file for the typechecker.
-->

### Details

- **Deadline April 13:**
    
    - The provided template already passes two good programs. Which programs are this? 
    - Find the functions `checkStm` and `inferTypeExp` in `TypeChecker.hs`. Which typechecking rules are already implemented?
      - Hint: Find and open the file `AbsCPP.hs`. Find the data types `Stm` and `Exp`. These are the data types of our abstract syntax tree. Each line in the definition of `Stm` or `Exp` corresponds to a line in the grammar. The typechecker works by induction (or recursion) over this data type. For each case in the data type there must be a corresponding case  in `checkStm` or `inferTypeExp`.
    - Find and open the program `easy_add.cc`. Which rule needs to be implemented to typecheck the program `easy_add.cc`?
      - Hint: This may look complicated at first sight, but notice that the typechecking for multiplication is already implemented in the template. 
      - After adding the typechecking rule for addition, you should get upon running `stack build` and `stack test`

            Good programs: passed 3 of 76 tests
            Bad programs:  passed 74 of 74 tests

    - Which two rules need to be implemented to typecheck the program `ass_easy.cc`?
      - Hint: If you run `stack exec CPPTypeChecker-exe ass_easy.cc` you will get a hint of what the missing rule it. 
      - Hint: To implement the rule, note that you can return a value without writing `return`. In fact, `return` is only needed if you want to typecast a value of type `Env` into a value of type `Err Env`. For now, you can more or less ignore the difference between `Env` and `Err Env`: While `Env` is the type of environments, a value of type `Err Env` is either an environment or an error message. 
    - After building and testing you should now get:
      
            Good programs: passed 5 of 76 tests
            Bad programs:  passed 73 of 74 tests

    - Remark: At the beginning you should focus on increasing the number of passed good programs.

- **Deadline April 20:** Implement the remaining cases until you pass all test programs. As shown above it may be good to pick a short and easy good test program and think about what is needed to make the typechecker pass it.


## Assignment Interpreter  

**(deadline tba)**

The assignment is detailed [here](http://www.grammaticalframework.org/ipl-book/assignments/assignment3/assignment3.html).

To implement the typechecker I suggest that you use Haskell.

Again we provided template files for Haskell and Scala. 

You can implement the interpreter without having a typechecker. (Just don't be surprised if curious things then happen if you run the interpreter on "bad" programs.)

In any case you need to test your interpreter on all the (good) test files, see [here](https://github.com/ChapmanCPSC/compiler-assignments/tree/master/Interpreter/Haskell/test/good) or [here](https://github.com/ChapmanCPSC/compiler-assignments/tree/master/Interpreter/Scala/src/test/good).




