package CPPScala.Absyn;
import CPP.Absyn.{
	Type        => JType, 
	Exp         => JExp,
	Stm         => JStm,
	SBlock      => JSBlock,
	SDecls      => JSDecls,
	SExp        => JSExp,
	SIfElse     => JSIfElse,
	SInit       => JSInit,
	SReturn     => JSReturn,
	SReturnVoid => JSReturnVoid,
	SWhile      => JSWhile,
	ListExp,
	ListStm,
	ListId
}
import scala.collection.JavaConverters._


sealed trait Stm extends ToJava[JStm] with PrettyPrint {
	def print = CPP.PrettyPrinter.print(this.toJava)
	def show  = CPP.PrettyPrinter.show(this.toJava)
}
case class SBlock(stms:List[Stm]) extends Stm {
	def toJava = {
		val stms1 = new ListStm()
		stms1.addAll(stms.map( x => x.toJava ).asJava)
		new JSBlock(stms1)
	}
}
case class SDecls(ty:Type, ids:List[String]) extends Stm {
	def toJava = {
		val ids1 = new ListId()
		ids1.addAll(ids.asJava)
		new JSDecls(ty.toJava, ids1)
	}
}
case class SExp(exp:Exp) extends Stm {
	def toJava = new JSExp(exp.toJava)
}
case class SIfElse(exp:Exp, stm1:Stm, stm2:Stm) extends Stm {
	def toJava = new JSIfElse(exp.toJava, stm1.toJava, stm2.toJava)
}
case class SInit(ty:Type, id:String, exp:Exp) extends Stm {
	def toJava = new JSInit(ty.toJava, id, exp.toJava)
}
case class SReturn(exp:Exp) extends Stm {
	def toJava = new JSReturn(exp.toJava)
}
case object SReturnVoid extends Stm {
	def toJava = new JSReturnVoid()
}
case class SWhile(exp:Exp, stm:Stm) extends Stm {
	def toJava = new JSWhile(exp.toJava, stm.toJava)
}


object Stm {
	def toScala(i : JStm) : Stm = i match {
		case s:JSBlock      => SBlock(s.liststm_.asScala.toList.map( x => toScala(x) ))
		case s:JSDecls      => SDecls(Type.toScala(s.type_), s.listid_.asScala.toList)
		case s:JSExp        => SExp(Exp.toScala(s.exp_))
		case s:JSIfElse     => SIfElse(Exp.toScala(s.exp_), toScala(s.stm_1), toScala(s.stm_2))
		case s:JSInit       => SInit(Type.toScala(s.type_), s.id_, Exp.toScala(s.exp_))
		case s:JSReturn     => SReturn(Exp.toScala(s.exp_))
		case _:JSReturnVoid => SReturnVoid
		case s:JSWhile      => SWhile(Exp.toScala(s.exp_), toScala(s.stm_))
	}
}
