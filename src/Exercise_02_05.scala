object CompositionExercise {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    def func1(i: Int): Int =
      i + 2
    def func2(j: Int): Int =
      j + 3

    var composedFunc = compose(func1, func2)
    println(composedFunc(10))
  }
}