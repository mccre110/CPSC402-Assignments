package CPPScala.Absyn;
import CPP.Absyn.{
	Type  => JType,
	Arg   => JArg,
	ADecl => JADecl
}

sealed trait Arg extends ToJava[JArg] with PrettyPrint {
	def print = CPP.PrettyPrinter.print(this.toJava)
	def show  = CPP.PrettyPrinter.show(this.toJava)
}
case class ADecl(ty:Type, id:String) extends Arg {
	def toJava = new JADecl(ty.toJava, id)
}


object Arg {
	def toScala(i : JArg) : Arg = i match {
		case a:JADecl => ADecl(Type.toScala(a.type_), a.id_)
	}
}