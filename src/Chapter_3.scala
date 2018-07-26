sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  /*  The result of the case statement will be 3, because the first case that matches is:
      Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y

    and so x = 1 and y = 2; x + y = 3
 */
  def testMatch(): Int = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x
  }

  def ex_3_1(): Unit =
    println(testMatch())

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, tail) => tail
  }

  def ex_3_2(): Unit = {
    val testList = List(1, 2, 3, 4, 5)
    val testTail = tail(testList)
    println(testTail.toString())
  }

  def main(args: Array[String]): Unit = {
    //ex_3_1()
    ex_3_2()
  }
}


