package CPPScala

import CPP.Yylex
import CPPScala._
import CPPScala.Absyn._
import java.io._
import java_cup.runtime._
import scala.util.{Try, Success, Failure}
import org.scalatest.FunSuite
import scala.io.Source


class InterpreterTests extends FunSuite {
  type TestIO = (List[Value], List[Value])
  class TestInterpreter(var io : TestIO) extends Interpreter {
    def printInt(i: Int): Unit = io match {
      case (ins,outs) => io = (ins, VInt(i)::outs) 
    }
    def printDouble(d: Double): Unit = io match {
      case (ins,outs) => io = (ins, VDouble(d)::outs) 
    }
    @throws[RuntimeException]
    def readInt(): Int = io match {
      case (VInt(i)::ins, outs) => {
        io = (ins, outs)
        i
      }
      case _ => throw RuntimeException("Invalid input given. expected an int.")
    }
    @throws[RuntimeException]
    def readDouble(): Double = io match {
      case (VDouble(d)::ins, outs) => {
        io = (ins, outs)
        d
      }
      case (VInt(i)::ins, outs) => {
        io = (ins, outs)
        i
      }
      case _ => throw RuntimeException("Invalid input given. expected a double.")
    }
  }
  
  test("Interpreting programs") {
    println("------------ Interpreting programs -----------")
    assert(test("src/test/good", typeCheckAndInterpret))
  }

  def parseFile(file:File) : Try[Program] = {
    var l : Yylex = null
    try { l = new Yylex(new FileReader(file)) } catch {
      case e:IOException => { 
        return Typechecker.fail(s"Error: File not found: $file")
      }
    }
    val p = new Parser(l)
    var prog : Program = null
    try { prog = p.parseProgram } catch {
      case e: Throwable => {
        return Typechecker.fail("At line " + 
          String.valueOf(l.line_num()) + 
          ", near \"" + l.buff() + "\" :\n"+
          "     " + e.getMessage())
      }
    }
    Success(prog)
  }

  @throws[RuntimeException]
  def parse(s: String): Value = {
    try VInt(s.toInt)
    catch { case _ : Throwable => try VDouble(s.toDouble)
    catch { case _ : Throwable => throw RuntimeException("Error parsing " + s) }}
  }

  @throws[RuntimeException]
  def getIO(file:File) : TestIO = {
    var ins = List[Value]()
    var outs = List[Value]()
    val inFile = file.getAbsolutePath + ".input"
    if (new File(inFile).isFile) {
      val source = Source.fromFile(inFile)
      ins = source.getLines.toList.map{ parse }
      source.close
    }
    val outFile = file.getAbsolutePath + ".output"
    if (new File(outFile).exists) {
      val source = Source.fromFile(outFile)
      outs = source.getLines.toList.map{ parse }
      source.close
    }
    (ins, outs)
  }
    
  def showIO(v:Value): String = v match {
    case VInt(i) => i.toString
    case VDouble(d) => d.toString
    case VVoid => "void"
    case VUndefined => "undefined"
  }
  
  def typeCheckAndInterpret(file:File) : Try[TestIO] = for {
    prog <- parseFile(file)
    _ <- Typechecker.typecheck(prog)
    (ins, outs) <- Try(getIO(file))
    i = new TestInterpreter((ins, List()))
    _ <- Try(i.exec(prog))
  } yield (outs, i.io._2.reverse)

  def test(fp:String, fun:File=>Try[TestIO]) : Boolean = {
    val progsDir = new File(fp).getAbsoluteFile()

    val progsList = progsDir.listFiles.filter(_.isFile).toList.filter { file =>
        file.getName.endsWith("cc")
    }
    var passing = true
    progsList.foreach {
      f => fun(f) match {
        case Success((expOut, compOut)) => {
          println("Expected output: ")
          expOut.foreach{ v => println(showIO(v)) }
          println("Actual output: ")
          compOut.foreach{ v => println(showIO(v)) }
          if (expOut == compOut) println(f +": passed")
          else { println(f +": failed"); passing = false }
        }
        case Failure(e) => { println(f + ": failed\n" + e.toString) ; passing = false }
      }
    }
    return passing
  }
}
