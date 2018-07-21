object FibonacciExercise {
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int =
      if (n <= 0) prev
      else go(n-1, curr, prev+curr)

    go(n, 0, 1)
  }

  private def formatFib(n: Int) = {
    val msg = "Fibonacci sequence member %d is %d."
    msg.format(n, fibonacci(n))
  }

  def main(args: Array[String]): Unit =
    println(formatFib(7))
}
