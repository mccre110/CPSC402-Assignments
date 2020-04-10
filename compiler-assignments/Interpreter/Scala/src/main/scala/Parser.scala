package CPPScala;
import CPPScala.Absyn._
import CPP.parser

class Parser(s:java_cup.runtime.Scanner) extends parser(s) {
  @throws[Exception]
  def parseProgram:Program = Program.toScala(super.pProgram())
}