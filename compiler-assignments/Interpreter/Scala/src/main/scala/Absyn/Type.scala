package CPPScala.Absyn;
import CPP.Absyn.{
	Type        => JType, 
	Type_bool   => JType_bool, 
	Type_double => JType_double, 
	Type_int    => JType_int, 
	Type_string => JType_string,
	Type_void   => JType_void
}

trait ToJava[T] {
	def toJava : T
}

trait PrettyPrint {
	def print : String
	def show : String
}

sealed trait Type extends ToJava[JType] with PrettyPrint {
	def print = CPP.PrettyPrinter.print(this.toJava)
	def show  = CPP.PrettyPrinter.show(this.toJava)
}
case object Type_bool extends Type {
	def toJava = new JType_bool()
}
case object Type_double extends Type {
	def toJava = new JType_double()
}
case object Type_int extends Type {
	def toJava = new JType_int()
}
case object Type_string extends Type {
	def toJava = new JType_string()
}
case object Type_void extends Type {
	def toJava = new JType_void()
}


object Type{
	def toScala(i : JType) : Type = i match {
		case _:JType_bool   => Type_bool
		case _:JType_double => Type_double
		case _:JType_int    => Type_int
		case _:JType_string => Type_string
		case _:JType_void   => Type_void
	}
}