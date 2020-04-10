package CPPScala.Absyn;
import CPP.Absyn.{
	Type    => JType, 
	Exp     => JExp,
	EAnd    => JEAnd,
	EApp    => JEApp,
	EAss    => JEAss,
	EDecr   => JEDecr,
	EDiv    => JEDiv,
	EDouble => JEDouble,
	EEq     => JEEq,
	EFalse  => JEFalse,
	EGt     => JEGt,
	EGtEq   => JEGtEq,
	EId     => JEId,
	EIncr   => JEIncr,
	EInt    => JEInt,
	ELt     => JELt,
	ELtEq   => JELtEq,
	EMinus  => JEMinus,
	ENEq    => JENEq,
	EOr     => JEOr,
	EPDecr  => JEPDecr,
	EPIncr  => JEPIncr,
	EPlus   => JEPlus,
	EString => JEString,
	ETimes  => JETimes,
	ETrue   => JETrue,
	ETyped  => JETyped,
	ListExp
}
import scala.collection.JavaConverters._


sealed trait Exp extends ToJava[JExp] with PrettyPrint {
	def print = CPP.PrettyPrinter.print(this.toJava)
	def show  = CPP.PrettyPrinter.show(this.toJava)
}

case class EAnd(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEAnd(exp1.toJava, exp2.toJava)
}
case class EApp(id:String, exps:List[Exp]) extends Exp {
	def toJava = {
		val exps1 = new ListExp()
		exps1.addAll(exps.map( x => x.toJava ).asJava)
		new JEApp(id, exps1)
	}
}
case class EAss(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEAss(exp1.toJava, exp2.toJava)
}
case class EDecr(exp:Exp) extends Exp {
	def toJava = new JEDecr(exp.toJava)
}
case class EDiv(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEDiv(exp1.toJava, exp2.toJava)
}
case class EDouble(d:Double) extends Exp {
	def toJava = new JEDouble(d)
}
case class EEq(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEEq(exp1.toJava, exp2.toJava)
}
case object EFalse extends Exp {
	def toJava = new JEFalse()
}
case class EGt(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEGt(exp1.toJava, exp2.toJava)
}
case class EGtEq(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEGtEq(exp1.toJava, exp2.toJava)
}
case class EId(id:String) extends Exp {
	def toJava = new JEId(id)
}
case class EIncr(exp:Exp) extends Exp {
	def toJava = new JEIncr(exp.toJava)
}
case class EInt(i:Integer) extends Exp {
	def toJava = new JEInt(i)
}
case class ELt(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JELt(exp1.toJava, exp2.toJava)
}
case class ELtEq(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JELtEq(exp1.toJava, exp2.toJava)
}
case class EMinus(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEMinus(exp1.toJava, exp2.toJava)
}
case class ENEq(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JENEq(exp1.toJava, exp2.toJava)
}
case class EOr(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEOr(exp1.toJava, exp2.toJava)
}
case class EPDecr(exp:Exp) extends Exp {
	def toJava = new JEPDecr(exp.toJava)
}
case class EPIncr(exp:Exp) extends Exp {
	def toJava = new JEPIncr(exp.toJava)
}
case class EPlus(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JEPlus(exp1.toJava, exp2.toJava)
}
case class EString(s:String) extends Exp {
	def toJava = new JEString(s)
}
case class ETimes(exp1:Exp, exp2:Exp) extends Exp {
	def toJava = new JETimes(exp1.toJava, exp2.toJava)
}
case object ETrue extends Exp {
	def toJava = new JETrue()
}
case class ETyped(exp:Exp, ty:Type) extends Exp {
	def toJava = new JETyped(exp.toJava, ty.toJava)
}


object Exp {
	def toScala(i : JExp) : Exp = i match {
		case e:JEAnd    => EAnd(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEApp    => EApp(e.id_, e.listexp_.asScala.toList.map( x => toScala(x) ))
		case e:JEAss    => EAss(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEDecr   => EDecr(toScala(e.exp_))
		case e:JEDiv    => EDiv(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEDouble => EDouble(e.double_)
		case e:JEEq     => EEq(toScala(e.exp_1), toScala(e.exp_2))
		case _:JEFalse  => EFalse
		case e:JEGt     => EGt(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEGtEq   => EGtEq(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEId     => EId(e.id_)
		case e:JEIncr   => EIncr(toScala(e.exp_))
		case e:JEInt    => EInt(e.integer_)
		case e:JELt     => ELt(toScala(e.exp_1), toScala(e.exp_2))
		case e:JELtEq   => ELtEq(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEMinus  => EMinus(toScala(e.exp_1), toScala(e.exp_2))
		case e:JENEq    => ENEq(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEOr     => EOr(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEPDecr  => EPDecr(toScala(e.exp_))
		case e:JEPIncr  => EPIncr(toScala(e.exp_))
		case e:JEPlus   => EPlus(toScala(e.exp_1), toScala(e.exp_2))
		case e:JEString => EString(e.string_)
		case e:JETimes  => ETimes(toScala(e.exp_1), toScala(e.exp_2))
		case _:JETrue   => ETrue
		case e:JETyped  => ETyped(toScala(e.exp_), Type.toScala(e.type_))

	}
}