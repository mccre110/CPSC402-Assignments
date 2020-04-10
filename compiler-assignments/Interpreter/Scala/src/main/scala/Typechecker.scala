

// please replace this file with your implementation of TypeChecker.scala from the previous assignment


package CPPScala;
import CPPScala.Absyn.{Program}
import scala.util.{Try, Success, Failure}

case class TypecheckingException(e: String) extends Exception(e)

object Typechecker {
    def fail[T](e:String) : Try[T] = Failure(TypecheckingException(e))
    def typecheck(program:Program) : Try[Unit] = Success(())
}