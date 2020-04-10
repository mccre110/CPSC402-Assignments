package CPPScala

import CPP.Yylex
import CPPScala._
import CPPScala.Absyn._
import java.io._
import java_cup.runtime._
import scala.util.{Try, Success, Failure}
import org.scalatest.FunSuite
import scala.Console

class TypecheckerAltTests extends FunSuite {
  println(Console.UNDERLINED + "\nThis is the test program for Programming Languages Lab 2 (TypecheckerAlt)\n" + Console.RESET)

  test("Testing bad programs") {
    assert(test("src/test/bad", typeCheckBad))
  }

  test("Testing good programs") {
    assert(test("src/test/good", typeCheckGood))
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

  def typeCheckBad(file:File) : Try[Unit] = for {
    prog <- parseFile(file)
    ret <- TypecheckerAlt.typecheck(prog) match {
        case Success(_) => Typechecker.fail("Should not typecheck")
        case _ => Typechecker.ok
      }
  } yield ret
  
  def typeCheckGood(file:File) : Try[Unit] = for {
    prog <- parseFile(file)
    ret <- TypecheckerAlt.typecheck(prog)
  } yield ret

  def test(fp:String, fun:File=>Try[Unit]) : Boolean = {
    val progsDir = new File(fp).getAbsoluteFile()

    val progsList = progsDir.listFiles.filter(_.isFile).toList.filter { file =>
        file.getName.endsWith("cc")
    }
    var passing = true
    progsList.foreach {
      f => fun(f) match {
        case Success(_) => ()
        case Failure(TypecheckingException(e)) => { 
          println("Type-checking " + f + " failed\n" + Console.RED + e.toString + Console.RESET)
          passing = false 
        } 
        case Failure(e) => { 
          println("Type-checking " + f + " failed\n" + Console.RED + e.toString + Console.RESET)
          passing = false 
        }
      }
    }
    return passing
  }
}
