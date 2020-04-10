package CPPScala

import CPP.Yylex
import CPPScala._
import CPPScala.Absyn._
import java.io._
import java_cup.runtime._
import scala.util.{Success, Failure}
import scala.io.StdIn.{readLine}


object Test {
  var l : Yylex = null
  var p : Parser = null
  
  @throws[Exception]
  def parse : Program = {
    val ast = p.parseProgram
    println()
    println("---------------------- Parsing ----------------------")
    println()
    println("[Abstract Syntax]")
    println()
    println(ast.show)
    println()
    println("[Linearized Tree]")
    println()
    println(ast.print)
    return ast
  }


  class IOInterpreter extends Interpreter {
    def printInt(i: Int): Unit = println(i)
    def printDouble(d: Double): Unit = println(d)
    @throws[RuntimeException]
    def readInt(): Int = try readLine().toInt catch {
      case _ : Throwable => throw RuntimeException("Invalid input given. expected an int.")
    }
    @throws[RuntimeException]
    def readDouble(): Double = try readLine().toDouble catch {
      case _ : Throwable => throw RuntimeException("Invalid input given. expected a double.")
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      var input : Reader = null
      if (args.length == 0) input = new InputStreamReader(System.in)
      else input = new FileReader(args(0))
      l = new Yylex(input)
    }
    catch {
      case e:IOException => { 
        System.err.println("Error: File not found: " + args(0))
        return;
      }
    }
    p = new Parser(l)

    var prog : Program = null
    try {
      prog = parse 
    } catch {
      case e: Throwable => {
        System.err.println("At line " + String.valueOf(l.line_num()) + ", near \"" + l.buff() + "\" :")
        System.err.println("     " + e.getMessage())
        return;
      }
    }

    println("-------------------- Typechecking --------------------\n")

    Typechecker.typecheck(prog) match {
      case Success(_) => {
        println("Succesfully typechecks.\n")
        println("-------------------- Interpreting --------------------\n")
        try{ new IOInterpreter().exec(prog) }
        catch {
          case RuntimeException(e) => return println("Error: " + e)
        }
        println("Done.")
      }
      case Failure(TypecheckingException(e)) => println(e)
      case Failure(e) => println(e)
    }
  }
}