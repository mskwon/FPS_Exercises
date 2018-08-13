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

  def ex_5_2(): Unit = {
    val testStream = Stream(1, 2, 3, 4, 5, 6)
    println(testStream.take(3).toList)
  }

  def main(args: Array[String]): Unit = {
    //ex_5_1()
    ex_5_2()
  }
}