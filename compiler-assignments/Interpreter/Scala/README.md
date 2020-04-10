# CPPInterpreterScala
An interpreter for a subset of C++ written in Scala

To run you need to install [SBT](https://www.scala-sbt.org/download.html), then inside the project run `sbt test` to run the test suite. (And run it twice if you get "error: error while loading String, class file '/modules/java.base/java/lang/String.class' is broken" ... this error should disappear the second time.) (To see or change what `sbt test` is doing look at ...)

To run typechecking on a specific file, run `sbt "run <file_path>.cc"`

src/main/java contains the files produced automatically by bnfc

src/main/scala conain the files adapted by hand from src/main/java


  