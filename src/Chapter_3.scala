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

  // Exercise 3.6
  /*  The method init cannot run in constant time because of the need to iterate through the linked list members until
      the last member is found (via pattern matching).
   */
  def init[A](l: List[A]): List[A] = l match{
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def ex_3_6(): Unit = {
    val testList = List(1, 2, 3, 4)
    println(init(testList))
  }

  // Exercise 3.7
  /*  With the current way foldRight is implemented, it is not possible for product2 to short-circuit on encountering a
      0 as there is nowhere for the short-circuiting logic to go. The short-circuiting could possibly be implemented
      via additional function argument, such that foldRight would terminate earlier should the function return true.
   */

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.8
  /*  It appears that perhaps the List constructor has a foldRight-like implementation
   */
  def ex_3_8(): Unit = {
    println(foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)))
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  def ex_3_9(): Unit = {
    var testList = List(1, 2, 3, 4)
    println(length(testList))
  }

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def ex_3_10(): Unit = {
    var testList = List(1, 2, 3, 4)
    println(foldLeft(testList, 0)(_ + _))
  }

  // Exercise 3.11
  def sum_foldLeft(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product_foldLeft(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def length_foldLeft(as: List[Int]): Int =
    foldLeft(as, 0)((y, _) => 1 + y)

  def ex_3_11(): Unit = {
    var testList = List(1, 2, 3, 4)
    var testList2 = List(1.0, 2.0, 3.0, 4.0)
    println(testList)
    println("The sum of the list is: %d".format(sum_foldLeft(testList)))
    println("The product of the list is: %f".format(product_foldLeft(testList2)))
    println("The length of the list is: %d".format(length_foldLeft(testList)))
  }

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))

  def ex_3_12(): Unit = {
    var testList = List(1, 2, 3)
    println(reverse(testList))
  }

  // Exercise 3.13

  def main(args: Array[String]): Unit = {
    //ex_3_1()
    //ex_3_2()
    //ex_3_3()
    //ex_3_4()
    //ex_3_5()
    //ex_3_6()
    //ex_3_8()
    //ex_3_9()
    //ex_3_10()
    //ex_3_11()
    ex_3_12()
  }
}


