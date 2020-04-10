package CPPScala;
import CPPScala.Absyn._
import scala.util.{Try, Success, Failure}
import scala.collection.immutable.HashMap
import cats._
import cats.implicits._

case class TypecheckingException(e: String) extends Exception(e)

object Typechecker {

    /** This is a wrapper for a String identifier in the AST, 
      * used for passing the id to the [[typeMismatchError]] method.
      */
    case class Id(s:String) extends PrettyPrint {
        def print = s
        def show = s
    }

    /** This is a wrapper for a list of terms we wan to pass to [[typeMismatchError]].
      * Used for reporting type mismatch errors for operators like `+`, which can be
      * typed as string/double/int.
      */
    case class Alternative[T <: PrettyPrint](vals:List[T]) extends PrettyPrint {
        def print = Foldable[List].intercalate(vals.map(i => i.print), "/").toString
        def show = ""
    }

    /** Abbreviation for `Success(())`
      */
    def ok : Try[Unit] = Success(())
    /** Wrapper for a failure that returns a [[TypecheckingException]].
      */
    def fail[T](e:String) : Try[T] = Failure(TypecheckingException(e))

    /** This is a helper function for pretty prenting a type mismatch error.
      * @param e the expression that is being typechecked
      * @param t1 the inferred type of the expression
      * @param t2 the expected type of the expression
      */
    def typeMismatchError(e:PrettyPrint, t1:PrettyPrint, t2:PrettyPrint): String = 
        "TYPE ERROR\n\n" +
        "Expected " + e.print + " to have type " + t1.print +
        " instead found type " + t2.print + "."

    /** This class represents a function type, usually written as (t1,...,tn) -> t
      * For example:
      * {{{FunctionType(List(Type_int), Type_void)}}}
      * corresponds to a function `f : (int) -> void` or more conventionally written as:
      * {{{ void f(int i){...} }}}
      */
    case class FunctionType(inputTypes:List[Type], outputType:Type) extends PrettyPrint {
        def print = "(" + Foldable[List].intercalate(inputTypes.map(i => i.print), ", ").toString + ") -> " + outputType.print
        def show = ""
    }

    /** The `Env`ironment holds a global context, which contains all the function signatures,
      * namely it is a mapping from the function name to its associated type, represented by
      * [[FunctionType]].
      * It also holds a local context, which is a list of maps, similar to the signatures map, 
      * which map variable identifiers to [[CPPScala.Absyn.Type]]. Since C/C++ allows variable
      * shadowing and we therefore (can) have multiple scopes within a method declaration. 
      * Each time there is a new code block, we add a new map to the stack and pop it off,
      * when we exit the current scope.
      * @param sig the global context
      * @param ctxt variable context 
      */
    case class Env(sig:HashMap[String, FunctionType] = HashMap(), ctxt:List[HashMap[String, Type]] = List(HashMap())) {
        def lookupFun(id:String) : Try[FunctionType] = sig.get(id) match {
            case Some(ty) => Success(ty)
            case None     => fail(s"TYPE ERROR\n\n$id was not declared.")
        }

        def insertFun(id:String, ty:FunctionType) : Try[Env] = sig.get(id) match {
            case Some(_) => fail(s"TYPE ERROR\n\nFailed to add $id to the symbol table, as it is already defined.")
            case None    => Success(Env(sig + ((id,ty)), ctxt))
        }

        /** Local contexts are stored as a stack of scope frames, with a new frame added when
          * entering a new scope. 
          * Looking up a variable requires checking the top-most frame first.
          * If the variable is not found, this does not mean that it is not inscope,
          * as we might have to look in the enclosing/above scope where the variable might
          * have been defined instead, e.g.:
          * {{{
          * void f(){
          *   int i = 1; 
          *   while(true){
          *     int j = i;
          *   } 
          * }
          * }}}
          * In the snippet above, whilst typechecking `int j = i`, we have to look up
          * the variable `i`, which is defined in the scope of the method `f` but not
          * in the scope of the while-block.
          * To find `i` this, we therefore have to look into the frame preceeding the
          * current one.
          */
        def lookupVar(id:String) : Try[Type] = {
            def lookupVarAux(x:List[HashMap[String, Type]], id:String) : Try[Type] = x match {
                case Nil    => fail(s"TYPE ERROR\n\n$id was not declared.")
                case c::ctx => c.get(id) match {
                    case Some(ty) => Success(ty)
                    case None => lookupVarAux(ctx, id)
                }
            }
            lookupVarAux(ctxt, id)
        }

        def insertVar(id:String, ty:Type) : Try[Env] = ctxt match {
            case Nil    => fail("Internal error, this should not happen.")
            case c::ctx => c.get(id) match {
                case Some(ty) => fail(s"TYPE ERROR\n\nFailed to add $id to the context, as it is already defined within this block.")
                case None => 
                    if(ty == Type_void) fail(s"TYPE ERROR\n\nCannot declare variable $id as void.")
                    else Success(Env(sig, c + ((id,ty)) :: ctx))
            }
        }
        /** @return adds a new empty frame to the stack of local contexts in `env`
          */
        def newBlock : Env = Env(sig,HashMap[String, Type]() :: ctxt)
    }


    /** The empty environment contains the type signatures for the four built in
      * functions:
      * {{{
      * void printInt(int i)
      * void printDouble(double d)
      * int readInt()
      * double readDouble()
      * }}}
      */
    def emptyEnv : Env = Env(HashMap(
            "printInt"    -> FunctionType(List(Type_int),    Type_void),
            "printDouble" -> FunctionType(List(Type_double), Type_void),
            "readInt"     -> FunctionType(List(), Type_int),
            "readDouble"  -> FunctionType(List(), Type_double)
        ))

    /** Builds the global/signature context by taking a list of [[CPPScala.Absyn.Def]]s, 
      * which are the AST representation of C/C++ methods and adding the name/id and 
      * corresponding [[FunctionType]] to the environment.
      */
    def buildEnv(defs:List[Def]) : Try[Env] = defs match {
        case Nil => Success(emptyEnv)
        case DFun(ty, id, args, _) :: xs => for {
            env1 <- buildEnv(xs)
            env2 <- env1.insertFun(id, FunctionType(args.map{ case ADecl(t,_) => t }, ty))
        } yield env2
    }

    /** This is the main type-checking method, which takes the whole progam, made up of 
      * a list of [[CPPScala.Absyn.Def]]initions. It first uses [[buildEnv]] to collect 
      * the type signatures of all the methods/functions and then runs [[checkDef]] 
      * on each one.
      */
    def typecheck(program:Program) : Try[Unit] = program match {
        case PDefs(Nil)  => fail("TYPE ERROR\n\nFile cannot be empty.")
        case PDefs(defs) => for {
            env <- buildEnv(defs)
            _   <- defs.traverse(x => checkDef(env,x))
        } yield ok
    }


    def checkDef(env:Env, d:Def) : Try[Unit] = d match {
        case DFun(ty, id, args, stms) => for {
            _    <- if(id == "main") checkMain(ty,args) else ok
            env1 <- Foldable[List].foldM(args,env) { case (e, ADecl(t,i)) => e.insertVar(i,t) }
            _    <- Foldable[List].foldM(stms,env1){ case (e, s) => checkStm(e, s, ty) }
        } yield ok
    }

    def checkMain(ty:Type, args:List[Arg]) : Try[Unit] = (ty,args) match {
        case (Type_int, Nil) => ok
        case (Type_int, xs)  => fail("TYPE ERROR\n\nError, main cannot have arguments.")
        case (ty, _)         => fail(typeMismatchError(Id("main"), Type_int, ty))
    }

    def checkStm(env:Env, stm:Stm, ty:Type) : Try[Env] = stm match {
        case SExp(e) => for{ _ <- inferTypeExp(env, e) } yield env
        case SDecls(ty1, ids) => for {
            env1 <- Foldable[List].foldM(ids,env){ case (e, id) => e.insertVar(id, ty1) } 
        } yield env1
        case SReturn(exp) => for { _ <- checkExp(env, exp, ty) } yield env
        /** here goes your code, delete the "catch all" statement below when your code is complete **/
        case e =>  fail("Missing case in checkStm encountered:\n" + e.print)
    }

    def inferTypeExp(env:Env, exp:Exp) : Try[Type] = exp match {
        case EAss(e1,e2) => for {
            ty <- inferTypeExp(env, e1)
            _  <- checkExp(env, e2, ty)
        } yield ty
        case ETyped(e,ty) => for { _ <- checkExp(env, e, ty) } yield ty
        /** here goes your code, delete the "catch all" statement below when your code is complete **/
        case e =>  fail("Missing case in inferTypeExp encountered:\n" + e.print)
    }

    def inferTypeOverloadedExp(env:Env, alts:List[Type], e:Exp, es:List[Exp]) : Try[Type] = for {
        ty <- inferTypeExp(env, e)
        _  <- if(!alts.contains(ty)) fail(typeMismatchError(e,Alternative(alts), ty)) else ok
        _  <- es.traverse(x => checkExp(env,x,ty))
    } yield ty


    def checkExp(env:Env, exp:Exp, ty:Type) : Try[Unit] = for {
        ty1 <- inferTypeExp(env, exp)
        _ <- if(ty == ty1) ok else fail(typeMismatchError(exp, ty, ty1)) 
    } yield () 

}