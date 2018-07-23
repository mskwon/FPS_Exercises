object isSortedExercise {
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

  def main(args: Array[String]): Unit = {
    println(formatIsSorted(Array(1,2,3), (x: Int, y: Int) => x <= y))
    println(formatIsSorted(Array(2,1,3), (x: Int, y: Int) => x <= y))
    println(formatIsSorted(Array("A", "B", "C"), (x: String, y: String) => x <= y))
    println(formatIsSorted(Array("A", "C", "B"), (x: String, y: String) => x <= y))
  }
}
