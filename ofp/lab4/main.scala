abstract class Regex {
  def asString(): String = this.simplify().stringify()

  def stringify(): String = this match {
    case Epsilon => "ε"
    case Symbol(ch @ ('|' | '*' | '(' | ')' | '\\')) => "\\" + ch.toString
    case Symbol(ch) => ch.toString
    case Alt(lhs, rhs) => lhs.stringify() + "|" + rhs.stringify()
    case Concat(lhs @ Alt(_, _), rhs @ Alt(_, _)) => "(" + lhs.stringify() + ")(" + rhs.stringify() + ")"
    case Concat(lhs @ Alt(_, _), rhs) => "(" + lhs.stringify() + ")" + rhs.stringify()
    case Concat(lhs, rhs @ Alt(_, _)) => lhs.stringify() + "(" + rhs.stringify() + ")"
    case Concat(lhs, rhs) => lhs.stringify() + "" + rhs.stringify()
    case Star(inner @ (Symbol(_) | Epsilon)) => inner.stringify() + "*"
    case Star(inner) => "(" + inner.stringify() + ")*"
  }

  def simplify(): Regex = this match {
    case Alt(lhs, rhs) => Alt(lhs.simplify(), rhs.simplify()) match {
      case Alt(l, r) if l == r => l
      case o => o
    }
    case Concat(lhs, rhs) => Concat(lhs.simplify(), rhs.simplify()) match {
      case Concat(l, Epsilon) => l
      case Concat(Epsilon, r) => r
      case o => o
    }
    case Star(inner) => Star(inner.simplify()) match {
      case Star(Epsilon) => Epsilon
      case Star(Star(i)) => Star(i)
      case o => o
    }
    case o => o
  }
}

case object Epsilon extends Regex
case class Symbol(c: Char) extends Regex
case class Alt(lhs: Regex, rhs: Regex) extends Regex
case class Concat(lhs: Regex, rhs: Regex) extends Regex
case class Star(inner: Regex) extends Regex

object Main {
  def main(args: Array[String]): Unit = {
    val expr = Concat(
      Concat(
        Star(Alt(Symbol('a'), Symbol('b'))),
        Star(Epsilon)
      ),
      Concat(
        Star(Concat(
          Symbol('c'),
          Concat(Symbol('d'), Epsilon)
        )),
        Concat(
          Star(Symbol('e')),
          Symbol('*')
        )
      )
    )
    
    println(expr.asString())
  }
}
