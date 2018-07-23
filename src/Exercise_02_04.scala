object UncurryingExercise {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit = {
    def function(x: Int): Int => Int =
      (y: Int) => x + y

    var uncurriedFunc = uncurry(function)
    println(uncurriedFunc(2, 3))
  }
}