case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Stream[A]()
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Stream[A]()
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((h, b) => p(h) && b)

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    this.foldRight(Stream[A]())((h, acc) => if (p(h)) Cons(() => h, () => acc) else acc)
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object Chapter_5{
  // Exercise 5.1
  def ex_5_1(): Unit = {
    val testStream = Stream(1, 2, 3, 4)
    println(testStream.toList)
  }

  // Exercise 5.2
  def ex_5_2(): Unit = {
    val testStream = Stream(1, 2, 3, 4, 5, 6)
    println(testStream.take(3).toList)
    println(testStream.take(8).toList)
  }

  // Exercise 5.3
  def ex_5_3(): Unit = {
    val testStream = Stream(2, 4, 6)
    val testStream2 = Stream(2, 4, 6, 1)
    println(testStream.takeWhile((x: Int) => x%2==0).toList)
    println(testStream2.takeWhile((x: Int) => x%2==0).toList)
  }

  // Exercise 5.4
  def ex_5_4(): Unit = {
    val testStream = Stream(1, 2, 3, 4, 5, 6, 7)
    val testStream2 = Stream(1, 2)
    println(testStream.forAll(x => {println("Running"); x < 3}))
    println(testStream2.forAll(_ < 3))
  }

  // Exercise 5.5
  def ex_5_5(): Unit = {
    val testStream = Stream(1, 2, 3, 4, 5, 6, 7)
    val testStream2 = Stream(1)
    println(testStream.takeWhile2(x => x < 3).toList)
    println(testStream2.takeWhile2(_ < 3).toList)
  }

  def main(args: Array[String]): Unit = {
    //ex_5_1()
    //ex_5_2()
    //ex_5_3()
    //ex_5_4()
    ex_5_5()
  }
}