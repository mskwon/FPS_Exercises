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

  // Exdrcise 3.3
  def setHead[A](head: A, as: List[A]): List[A] = as match {
    case Nil => Cons(head, Nil)
    case Cons(_, tail) => Cons(head, tail)
  }

  def ex_3_3(): Unit = {
    val testList = List(1, 2)
    println(setHead(2, testList).toString())
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n-1)
    case _ => l
  }

  def ex_3_4(): Unit = {
    val testList = List(1, 2, 3, 4, 5)
    println(drop(testList, 3).toString())
    println(drop(testList, 6).toString())
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def ex_3_5(): Unit = {
    val testList = List(2, 4, 1, 2, 3)
    println(dropWhile(testList, (x: Int) => x%2==0))
  }

  def main(args: Array[String]): Unit = {
    //ex_3_1()
    //ex_3_2()
    //ex_3_3()
    //ex_3_4()
    ex_3_5()
  }
}


