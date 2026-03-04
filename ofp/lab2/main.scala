
class BooleanFormula private(val op: String, val left: BooleanFormula, val right: BooleanFormula, val name: String) { 
  def this(variable: String) = this("var", null, null, variable)
  private def this(op: String, left: BooleanFormula, right: BooleanFormula) = this(op, left, right, null)

  def *(other: BooleanFormula): BooleanFormula = new BooleanFormula("*", this, other)
  def +(other: BooleanFormula): BooleanFormula = new BooleanFormula("+", this, other)
  def unary_! : BooleanFormula = new BooleanFormula("!", this, null)

  def eval(table: Map[String, Boolean]): Boolean = {
    val f: BooleanFormula => Boolean = {
      case bf if bf.op == "var" => table.getOrElse(bf.name, false)
      case bf if bf.op == "*"   => bf.left.eval(table) && bf.right.eval(table)
      case bf if bf.op == "+"   => bf.left.eval(table) || bf.right.eval(table)
      case bf if bf.op == "!"   => !bf.left.eval(table)
      case _ => false
    }
    f(this)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val a = BooleanFormula("a")
    val b = BooleanFormula("b")
    val c = BooleanFormula("c")
    println((a * !b).eval(Map("a" -> true, "b" -> false, "c" -> false)))
  }
}
