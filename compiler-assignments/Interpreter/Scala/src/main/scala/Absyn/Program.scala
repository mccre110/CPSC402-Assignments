package CPPScala.Absyn;
import CPP.Absyn.{
	Def     => JDef,
	Program => JProgram,
	PDefs   => JPDefs,
	ListDef
}
import scala.collection.JavaConverters._


sealed trait Program extends ToJava[JProgram] with PrettyPrint {
	def print = CPP.PrettyPrinter.print(this.toJava)
	def show  = CPP.PrettyPrinter.show(this.toJava)
}

case class PDefs(defs:List[Def]) extends Program {
	def toJava = {
		val defs1 = new ListDef()
		defs1.addAll(defs.map( x => x.toJava ).asJava)
		new JPDefs(defs1)
	}
}


object Program {
	def toScala(i : JProgram) : Program = i match {
		case p:JPDefs => PDefs(p.listdef_.asScala.toList.map( x => Def.toScala(x) ))
	}
}