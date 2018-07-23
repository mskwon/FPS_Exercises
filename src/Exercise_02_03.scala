object CurryingExercise {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def main(args: Array[String]): Unit = {
    def function(x: Int, y: Int): Int =
      x + y

    var curried_func = curry(function)
    var intermediate_func = curried_func(3)
    println(intermediate_func(2))
  }
}