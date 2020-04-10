package CPPScala;
import CPPScala.Absyn._
import scala.util.{Try, Success, Failure}
import scala.collection.immutable.HashMap
import cats._
import cats.implicits._

case class RuntimeException(e: String) extends Exception(e)

sealed trait Value
case class VInt(i:Int) extends Value
case class VDouble(d:Double) extends Value
case object VVoid extends Value
case object VUndefined extends Value


trait Interpreter {
  type Sig = HashMap[String, Def]
  type Context = HashMap[String, Value]
  type Env = (Sig, List[Context])

  private var env: Env = (HashMap(), List(HashMap()))
  def printInt(i: Int): Unit
  def printDouble(d: Double): Unit
  @throws[RuntimeException]
  def readInt(): Int
  @throws[RuntimeException]
  def readDouble(): Double

  final def extendSig(d:Def) = (d,env) match {
    case (DFun(_, i, _, _), (sig, ctxt)) => env = (sig + ((i, d)), ctxt)
  }

  @throws[RuntimeException]
  final def lookupSig(id: String): Def = env match {
    case (sig,_) => sig.get(id) match {
        case Some(f) => f
        case None    => throw TypecheckingException(s"Error, could not find $id.")
      }
  }

  final def extendContext(id: String, v: Value): Unit = env match {
    case (sig, Nil) => env = (sig, List(HashMap((id, v))))
    case (sig, c::txt) => env = (sig, c + ((id, v))::txt)
  }

  @throws[RuntimeException]
  private final def updateContextAux (id: String, v: Value, c:List[Context]): List[Context] = c match {
    case Nil => throw RuntimeException(s"Internal error, $id could not be found.")
    case c::txt => c.get(id) match {
        case Some(_) => c + ((id, v))::txt
        case None    => c::updateContextAux(id, v, txt)
      }
  }

  @throws[RuntimeException]
  final def updateContext (id: String, v: Value): Unit = env match {
    case (sig, ctxt) => env = (sig, updateContextAux(id, v, ctxt))
  }

  @throws[RuntimeException]
  private final def lookupContextAux(id: String, c:List[Context]): Value = c match {
    case Nil => throw RuntimeException(s"Internal error, $id could not be found.")
    case c::txt => c.get(id) match {
        case Some(f) => f
        case None    => lookupContextAux(id, txt)
      }
  }

  @throws[RuntimeException]
  final def lookupContext(id: String): Value = env match {
    case (sig, ctxt) => lookupContextAux(id, ctxt)
  }


  final def push(): Unit = env match {
    case (sig, ctxt) => env = (sig, HashMap[String,Value]()::ctxt)
  }

  @throws[RuntimeException]
  final def pop(): Unit = env match {
    case (sig, Nil) => throw RuntimeException("Internal error, can't pop an enpty context.")
    case (sig, _::ctxt) => env = (sig, ctxt)
  }

  @throws[RuntimeException]
  final def pushPop[A](f : () => A): A = {
    push; val a = f(); pop; return a
  }

  @throws[RuntimeException]
  final def exec(p: Program): Unit = p match {
    case PDefs(defs) => {
      env = (HashMap(), List(HashMap()))
      defs.foreach{ extendSig }
      val DFun(_,_,_,stms) = lookupSig("main")
      evalStms(stms)
      ()
    }
  }

  @throws[RuntimeException]
  final def evalStms(stm: List[Stm]): Option[Value] = stm match {
    case Nil => None
    case s::tms => {
      val v = evalStm(s)
      if(v == None) evalStms(tms) else v
    }
  }

  @throws[RuntimeException]
  final def evalStm(s: Stm): Option[Value] = s match {
    case SReturn(e) => Some(evalExp(e))

    /* missing cases go here */

    case _ => throw RuntimeException("Missing cases in evalStm.")
  }

  @throws[RuntimeException]
  final def evalExp(e: Exp): Value = e match {
    case EInt(i) => VInt(i)

    /* missing cases go here */
    
    case _ => throw RuntimeException("Missing cases in evalExp.")
  }

  @throws[RuntimeException]
  final def addValue(v1: Value, v2: Value): Value = (v1, v2) match {
    case (VInt(u),    VInt(v))    => VInt(u+v)
    case (VDouble(u), VDouble(v)) => VDouble(u+v)
    case (VDouble(u), VInt(v))    => VDouble(u+v)
    case (VInt(u),    VDouble(v)) => VDouble(u+v)
    case _ => throw RuntimeException("Internal error, trying to add incompatible types.")
  }

  @throws[RuntimeException]
  def subValue(v1: Value, v2: Value): Value = (v1, v2) match {
    case (VInt(u),    VInt(v))    => VInt(u-v)
    case (VDouble(u), VDouble(v)) => VDouble(u-v)
    case (VDouble(u), VInt(v))    => VDouble(u-v)
    case (VInt(u),    VDouble(v)) => VDouble(u-v)
    case _ => throw RuntimeException("Internal error, trying to sub incompatible types.")
  }

  @throws[RuntimeException]
  def mulValue(v1: Value, v2: Value): Value = (v1, v2) match {
    case (VInt(u),    VInt(v))    => VInt(u*v)
    case (VDouble(u), VDouble(v)) => VDouble(u*v)
    case (VDouble(u), VInt(v))    => VDouble(u*v)
    case (VInt(u),    VDouble(v)) => VDouble(u*v)
    case _ => throw RuntimeException("Internal error, trying to mul incompatible types.")
  }
  
  @throws[RuntimeException]
  def divValue(v1: Value, v2: Value): Value = (v1, v2) match {
    case (VInt(u),    VInt(v))    => if(v != 0) VInt(u/v) else throw RuntimeException("Error division by 0.")
    case (VDouble(u), VDouble(v)) => if(v != 0) VDouble(u/v) else throw RuntimeException("Error division by 0.")
    case (VDouble(u), VInt(v))    => if(v != 0) VDouble(u/v) else throw RuntimeException("Error division by 0.")
    case (VInt(u),    VDouble(v)) => if(v != 0) VDouble(u/v) else throw RuntimeException("Error division by 0.")
    case _ => throw RuntimeException("Internal error, trying to div incompatible types.")
  }

  @throws[RuntimeException]
  def ltValue(v1: Value, v2: Value): Value = (v1, v2) match {
    case (VInt(u),    VInt(v))    => if(u < v) VInt(1) else VInt(0)
    case (VDouble(u), VDouble(v)) => if(u < v) VInt(1) else VInt(0)
    case (VDouble(u), VInt(v))    => if(u < v) VInt(1) else VInt(0)
    case (VInt(u),    VDouble(v)) => if(u < v) VInt(1) else VInt(0)
    case _ => throw RuntimeException("Internal error, trying to apply ltValue to incompatible types.")
  }

  @throws[RuntimeException]
  def gtValue(v1: Value, v2: Value): Value = (v1, v2) match {
    case (VInt(u),    VInt(v))    => if(u > v) VInt(1) else VInt(0)
    case (VDouble(u), VDouble(v)) => if(u > v) VInt(1) else VInt(0)
    case (VDouble(u), VInt(v))    => if(u > v) VInt(1) else VInt(0)
    case (VInt(u),    VDouble(v)) => if(u > v) VInt(1) else VInt(0)
    case _ => throw RuntimeException("Internal error, trying to apply ltValue to incompatible types.")
  }

  @throws[RuntimeException]
  def negValue(v: Value): Value = v match {
    case VInt(0) => VInt(1)
    case VInt(1) => VInt(0)
    case _ => throw RuntimeException( "Internal error, trying to apply negValue to incompatible types.") 
  }
}