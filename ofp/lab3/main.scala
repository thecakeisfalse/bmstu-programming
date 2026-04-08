
class Formula[T] private(val eval: Map[String, T] => T)

object Formula {
  def variable[T](name: String): Formula[T] = new Formula[T](m => m(name))
  def const[T](value: T): Formula[T] = new Formula[T](_ => value)

  implicit class NumOps[T](val s: Formula[T])(implicit num: Numeric[T]) {
    def +(o: Formula[T]) = new Formula[T](m => num.plus(s.eval(m), o.eval(m)))
    def -(o: Formula[T]) = new Formula[T](m => num.minus(s.eval(m), o.eval(m)))
    def *(o: Formula[T]) = new Formula[T](m => num.times(s.eval(m), o.eval(m)))
  }

  implicit class FracOps[T](val s: Formula[T])(implicit frac: Fractional[T]) {
    def /(o: Formula[T]) = new Formula[T](m => frac.div(s.eval(m), o.eval(m)))
  }

  implicit class StrOps(val s: Formula[String]) {
    def +(o: Formula[String]) = new Formula[String](m => s.eval(m) + o.eval(m))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val x = Formula.variable[Int]("x")
    val y = Formula.variable[Int]("y")
    val f = (x + Formula.const(2)) * y - x
    println(f.eval(Map("x" -> 3, "y" -> 4)))

    val a = Formula.variable[Double]("a")
    val b = Formula.variable[Double]("b")
    val g = (a + b) / Formula.const(2.0)
    println(g.eval(Map("a" -> 1.0, "b" -> 3.0)))

    val s = Formula.variable[String]("s")
    val t = Formula.variable[String]("t")
    val h = s + Formula.const(", ") + t
    println(h.eval(Map("s" -> "hello", "t" -> "world")))
  }
}
