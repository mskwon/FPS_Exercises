object Chapter_2 {
  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int =
      if (n <= 0) prev
      else go(n-1, curr, prev+curr)

    go(n, 0, 1)
  }

  private def formatFib(n: Int) = {
    val msg = "Fibonacci sequence member %d is %d."
    msg.format(n, fib(n))
  }

  def ex_2_1() : Unit =
    println(formatFib(7))

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n+1 >= as.length) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  private def formatIsSorted[A](as: Array[A], ordered: (A, A) => Boolean) =
    s"[${as.mkString(" ")}] is ${if (isSorted(as, ordered)) "" else "not "}sorted."

  def ex_2_2(): Unit = {
    println(formatIsSorted(Array(1,2,3), (x: Int, y: Int) => x <= y))
    println(formatIsSorted(Array(2,1,3), (x: Int, y: Int) => x <= y))
    println(formatIsSorted(Array("A", "B", "C"), (x: String, y: String) => x <= y))
    println(formatIsSorted(Array("A", "C", "B"), (x: String, y: String) => x <= y))
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def ex_2_3(): Unit = {
    def function(x: Int, y: Int): Int =
      x + y

    var curried_func = curry(function)
    var intermediate_func = curried_func(3)
    println(intermediate_func(2))
  }

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def ex_2_4(): Unit = {
    def function(x: Int): Int => Int =
      (y: Int) => x + y

    var uncurriedFunc = uncurry(function)
    println(uncurriedFunc(2, 3))
  }

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def ex_2_5(): Unit = {
    def func1(i: Int): Int =
      i + 2
    def func2(j: Int): Int =
      j + 3

    var composedFunc = compose(func1, func2)
    println(composedFunc(10))
  }

  def main(args: Array[String]): Unit = {
    //ex_2_1()
    //ex_2_2()
    //ex_2_3()
    //ex_2_4()
    ex_2_5()
  }


}
