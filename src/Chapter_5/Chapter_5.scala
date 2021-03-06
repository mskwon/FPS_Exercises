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

  // Exercise 5.6
  def headOption2: Option[A] =
    this.foldRight(None: Option[A])((h, _) => Some(h))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    this.foldRight(Stream[B]())((h, t) => Cons(() => f(h), () => t))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(Stream[A]())((h, t) =>
      if (f(h)) Cons(() => h, () => t)
      else t
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    this.foldRight(s)((h, t) => Cons(() => h, () => t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream[B]())((h, acc) => f(h).append(acc))

  // Exercise 5.13
  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s match {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    })

  def take2(n: Int): Stream[A] =
    Stream.unfold((n, this))(s => s._2 match {
      case Cons(h, t) if s._1 > 0 => Some((h(), (s._1 - 1, t())))
      case _ => None
    })

  def takeWhile3(f: A => Boolean): Stream[A] =
    Stream.unfold(this)(s => s match {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    })

  def zipWith[B >: A](s2: Stream[B])(f: (B, B) => B): Stream[B] =
    Stream.unfold((this, s2))(s => s._1 match {
      case Cons(h1, t1) =>  s._2 match {
        case Cons(h2, t2) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => Some((h1(), (t1(), Empty)))
      }
      case Empty => s._2 match {
        case Cons(h2, t2) => Some((h2(), (Empty, t2())))
        case _ => None
      }
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, s2))(s => s._1 match {
      case Cons(h1, t1) => s._2 match {
        case Cons(h2, t2) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
        case _ => Some(((Some(h1()), None), (t1(), Empty)))
      }
      case _ => s._2 match {
        case Cons(h2, t2) => Some(((None, Some(h2())), (Empty, t2())))
        case _ => None
      }
    })

  // Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).filter(oh => (oh._2 != None)).foldRight(true)((oh, acc) => (oh._1 == oh._2) && acc)

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    Stream.unfold(this)(s => s match {
      case Cons(h, t) => Some(s, t())
      case Empty => None
    })

  // Exercise 5.16
  def scanRight[B>:A](z: B)(f: (B, B) => B): Stream[B] = this match {
    case Empty => Stream(z)
    case Cons(h, t) => {
      lazy val t_val = t().scanRight(z)(f)
      Cons(() => f(h(), t_val.headOption.get), () => t_val)
    }
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match
  {
    case None => Stream[A]()
    case Some((v, s)) => Stream.cons(v, unfold(s)(f))
  }
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

  // Exercise 5.6
  def ex_5_6(): Unit = {
    val testStream = Stream(1, 2, 3)
    val testStream2 = Stream()
    println(testStream.headOption2)
    println(testStream2.headOption2)
  }

  // Exercise 5.7
  def ex_5_7(): Unit = {
    val testStream = Stream(1, 2, 3)
    val testStream2 = Stream(4, 5, 6)

    println("map:")
    println(testStream.map(_ + 10).toList)

    println("\nfilter:")
    println(testStream.filter(_%2 == 0).toList)

    println("\nappend:")
    println(testStream.append(testStream2).toList)

    println("\nflatMap:")
    val testFunction = (x:Int) => Stream(10, 20, 30).map(_ + x)
    println(testStream.flatMap(testFunction).toList)
  }

  // Exercise 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def ex_5_8(): Unit = {
    println(constant(3).take(5).toList)
  }

  // Exercise 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

  def ex_5_9(): Unit = {
    println(from(3).take(5).toList)
  }

  // Exercise 5.10
  def fibs(n_1: Int, n_2: Int): Stream[Int] =
    Stream.cons(n_1, Stream.cons(n_2, fibs(n_1 + n_2, n_1 + 2*n_2)))

  def ex_5_10(): Unit =
    println(fibs(0,1).take(7).toList)

  // Exercise 5.11
  def ex_5_11(): Unit =
    println(Stream.unfold(1)(x => Some((x*2, x + 1))).take(5).toList)

  // Exercise 5.12
  def fibs2(): Stream[Int] =
    Stream.unfold(0::1::Nil)((l: List[Int]) => l match {
      case x_0::x_1::Nil => Some(x_0, x_1::x_0 + x_1::Nil)
    })

  def from2(n: Int): Stream[Int] =
    Stream.unfold(n)(x => Some(x, x+1))

  def constant2(n: Int): Stream[Int] =
    Stream.unfold(n)(x => Some(x, x))

  def ones2(): Stream[Int] =
    Stream.unfold(1)(x => Some(1, 1))

  def ex_5_12(): Unit = {
    println("fibs:")
    println(fibs2().take(7).toList)

    println("\nfrom:")
    println(from2(3).take(5).toList)

    println("\nconstant:")
    println(constant2(4).take(5).toList)

    println("\nones2:")
    println(ones2().take(5).toList)
  }

  // Exercise 5.13
  def ex_5_13(): Unit = {
    val testStream = Stream(1, 2, 3)
    val testStream2 = Stream(10, 10, 10)
    val testStream3 = Stream(10, 10)

    println("map:")
    println(testStream.map2(x => x + 10).toList)

    println("\ntake:")
    println(testStream.take2(2).toList)

    println("\ntakeWhile:")
    println(testStream.takeWhile3(x => x < 3).toList)

    println("\nzipWith:")
    println(testStream.zipWith(testStream2)((x, y) => x + y).toList)
    println(testStream.zipWith(testStream3)((x, y) => x + y).toList)

    println("\nzipAll:")
    println(testStream.zipAll(testStream2).toList)
    println(testStream.zipAll(testStream3).toList)
  }

  // Exercise 5.14
  def ex_5_14(): Unit = {
    val testStream = Stream(1,2,3)
    val testStream2 = Stream(1,2)
    val testStream3 = Stream(1,3)
    val testStream4 = Stream(1,2,3,4)

    println(testStream startsWith testStream2)
    println(testStream startsWith testStream3)
    println(testStream startsWith testStream4)
  }

  // Exercise 5.15
  def ex_5_15(): Unit = {
    val testStream = Stream(1,2,3)
    println(testStream.tails.toList.map(_.toList))
  }

  // Exercise 5.16
  def ex_5_16(): Unit = {
    val testStream = Stream(1,2,3)
    println(testStream.scanRight(0)(_ + _).toList)
  }
  /*  It is not possible to write scanRight using unfold because unfold cannot refer to intermediate values as the
      output Stream is generated. It may be possible to generate the correct output using unfold however the time
      complexity will not be O(N). It should be possible to implement scanRight using foldRight.
   */

  def main(args: Array[String]): Unit = {
    //ex_5_1()
    //ex_5_2()
    //ex_5_3()
    //ex_5_4()
    //ex_5_5()
    //ex_5_6()
    //ex_5_7()
    //ex_5_8()
    //ex_5_9()
    //ex_5_10()
    //ex_5_11()
    //ex_5_12()
    //ex_5_13()
    //ex_5_14()
    //ex_5_15()
    ex_5_16()
  }
}