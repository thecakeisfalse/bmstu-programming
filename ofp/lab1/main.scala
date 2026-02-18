object Main {
  def sumDigits: (Int, Int => (Int, Int)) => Int = {
    case (0, _) => 0
    case (n, f) if n < 0 => sumDigits(-n, f)
    case (n, f) => {
      val (d, k) = f(n)
      sumDigits(k, f) + d
    }
  }

  val f = (n: Int) => (n % 10, n / 10)
  val g = (n: Int) => (n % 16, n / 16)
  val u = (n: Int) => (n % 2, n / 2)

  def main(args: Array[String]): Unit = {
    println(sumDigits(12, f))      // 3
    println(sumDigits(0, f))       // 0
    println(sumDigits(-12, f))     // 3

    println(sumDigits(256, g))     // 1
    println(sumDigits(256, f))     // 13

    println(sumDigits(1 << 12, u)) // 1
    println(sumDigits(255, u))     // 8
    println(sumDigits(0, u))       // 0
  }
}
