package CPPScala;
import CPPScala.Absyn._
import scala.util.{Try, Success, Failure}
import scala.collection.immutable.HashMap
import cats._
import cats.implicits._


object TypecheckerAlt {

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


    /** This is a helper function for pretty prenting a type mismatch error.
      * @param e the expression that is being typechecked
      * @param t1 the inferred type of the expression
      * @param t2 the expected type of the expression
      */
    def typeMismatchError(e:PrettyPrint, t1:PrettyPrint, t2:PrettyPrint): TypecheckingException = 
        TypecheckingException("TYPE ERROR\n\n" +
        "Expected " + e.print + " to have type " + t1.print +
        " instead found type " + t2.print + ".")

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
        @throws[TypecheckingException]
        def lookupFun(id:String) : FunctionType = sig.get(id) match {
            case Some(ty) => return ty
            case None     => throw TypecheckingException(s"TYPE ERROR\n\n$id was not declared.")
        }
        @throws[TypecheckingException]
        def insertFun(id:String, ty:FunctionType) : Env = sig.get(id) match {
            case Some(_) => throw TypecheckingException(s"TYPE ERROR\n\nFailed to add $id to the symbol table, as it is already defined.")
            case None    => return Env(sig + ((id,ty)), ctxt)
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
        @throws[TypecheckingException]
        def lookupVar(id:String) : Type = {
            @throws[TypecheckingException]
            def lookupVarAux(x:List[HashMap[String, Type]], id:String) : Type = x match {
                case Nil    => throw TypecheckingException(s"TYPE ERROR\n\n$id was not declared.")
                case c::ctx => c.get(id) match {
                    case Some(ty) => return ty
                    case None => lookupVarAux(ctx, id)
                }
            }
            lookupVarAux(ctxt, id)
        }

        @throws[TypecheckingException]
        def insertVar(id:String, ty:Type) : Env = ctxt match {
            case Nil    => throw TypecheckingException("Internal error, this should not happen.")
            case c::ctx => c.get(id) match {
                case Some(ty) => throw TypecheckingException(s"TYPE ERROR\n\nFailed to add $id to the context, as it is already defined within this block.")
                case None => 
                    if(ty == Type_void) throw TypecheckingException(s"TYPE ERROR\n\nCannot declare variable $id as void.")
                    else return Env(sig, c + ((id,ty)) :: ctx)
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
    @throws[TypecheckingException]
    def buildEnv(defs:List[Def]) : Env = defs match {
        case Nil => return emptyEnv
        case DFun(ty, id, args, _) :: xs => {
            val env1 = buildEnv(xs)
            return env1.insertFun(id, FunctionType(args.map{ case ADecl(t,_) => t }, ty))
        }
    }

    /** This is the main type-checking method, which takes the whole progam, made up of 
      * a list of [[CPPScala.Absyn.Def]]initions. It first uses [[buildEnv]] to collect 
      * the type signatures of all the methods/functions and then runs [[checkDef]] 
      * on each one.
      */
    @throws[TypecheckingException]
    def typecheckAux(program:Program) : Unit = program match {
        case PDefs(Nil)  => throw TypecheckingException("TYPE ERROR\n\nFile cannot be empty.")
        case PDefs(defs) => {
            val env = buildEnv(defs)
            defs.foreach(x => checkDef(env,x))
        }
    }

    def typecheck(program:Program) : Try[Unit] = Try(typecheckAux(program))

    @throws[TypecheckingException]
    def checkDef(env:Env, d:Def) : Unit = d match {
        case DFun(ty, id, args, stms) => {
            if(id == "main") checkMain(ty,args)
            val env1 = args.foldLeft(env) { case (e, ADecl(t,i)) => e.insertVar(i,t) }
            stms.foldLeft(env1){ case (e, s) => checkStm(e, s, ty) }
            return
        }
    }

    @throws[TypecheckingException]
    def checkMain(ty:Type, args:List[Arg]) : Unit = (ty,args) match {
        case (Type_int, Nil) => return
        case (Type_int, xs)  => throw TypecheckingException("TYPE ERROR\n\nError, main cannot have arguments.")
        case (ty, _)         => throw typeMismatchError(Id("main"), Type_int, ty)
    }

    @throws[TypecheckingException]
    def checkStm(env:Env, stm:Stm, ty:Type) : Env = stm match {
        case SExp(e) => { 
            inferTypeExp(env, e)
            return env
        }
        case SDecls(ty1, ids) =>
                return ids.foldLeft(env){ case (e, id) => e.insertVar(id, ty1) } 
        case SReturn(exp) => { 
            checkExp(env, exp, ty)
            return env
        }
        case s => throw TypecheckingException("Missing case in checkStm encountered:\n" + s.print)
    }


    @throws[TypecheckingException]
    def inferTypeExp(env:Env, exp:Exp) : Type = exp match {
        case EAss(e1,e2) => {
            val ty = inferTypeExp(env, e1)
            checkExp(env, e2, ty)
            return ty
        }
        case ETyped(e,ty) => { 
            checkExp(env, e, ty)
            return ty
        }
        /** here goes your code, delete the "catch all" statement below when your code is complete **/
        case e  => throw TypecheckingException("Missing case in inferTypeExp encountered:\n" + e.print)
    }

    @throws[TypecheckingException]
    def inferTypeOverloadedExp(env:Env, alts:List[Type], e:Exp, es:List[Exp]) : Type = {
        val ty = inferTypeExp(env, e)
        if(!alts.contains(ty)) throw typeMismatchError(e,Alternative(alts), ty)
        es.foreach(x => checkExp(env,x,ty))
        return ty
    }

    @throws[TypecheckingException]
    def checkExp(env:Env, exp:Exp, ty:Type) : Unit = {
        val ty1 = inferTypeExp(env, exp)
        if(ty != ty1) throw typeMismatchError(exp, ty, ty1)
        return
    }

}