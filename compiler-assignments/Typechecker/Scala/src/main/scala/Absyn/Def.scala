package CPPScala.Absyn;
import CPP.Absyn.{
	Type => JType,
	Arg  => JArg,
	Stm  => JStm,
	Def  => JDef,
	DFun => JDFun,
	ListArg,
	ListStm
}
import scala.collection.JavaConverters._


sealed trait Def extends ToJava[JDef] with PrettyPrint {
	def print = CPP.PrettyPrinter.print(this.toJava)
	def show  = CPP.PrettyPrinter.show(this.toJava)
}

case class DFun(ty:Type, id:String, args:List[Arg], stms:List[Stm]) extends Def {
	def toJava = {
		val args1 = new ListArg()
		args1.addAll(args.map( x => x.toJava ).asJava)
		val stms1 = new ListStm()
		stms1.addAll(stms.map( x => x.toJava ).asJava)
		new JDFun(ty.toJava, id, args1, stms1)
	}
}


object Def {
	def toScala(i : JDef) : Def = i match {
		case d:JDFun => DFun(
			Type.toScala(d.type_), 
			d.id_, 
			d.listarg_.asScala.toList.map( x => Arg.toScala(x) ), 
			d.liststm_.asScala.toList.map( x => Stm.toScala(x) ))
	}
}