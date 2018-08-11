case class Some[+A](val get: A) extends Option[A]
case object None extends Option[Nothing]

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

// Exercise 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(x) => x
      case None => default
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this == None) ob
    else this
  }

  def filter(f: A => Boolean): Option[A] =
    if (map(f).getOrElse(false)) this
    else None
}

// Exercise 4.6
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e): Either[E, B]
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this.map(f) match {
      case Left(e) => Left(e): Either[EE, B]
      case Right(re) => re
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(e) => Left(e): Either[EE, C]
    case Right(a_val) => b match {
      case Left(f) => Left(f): Either[EE, C]
      case Right(b_val) => Right(f(a_val, b_val))
    }
  }
}


object Chapter_4 {
  def ex_4_1(): Unit = {
    val testSome = Some(1)
    val testSome2 = Some(2)
    val testNone = None
    val testFunction = (x: Int) => if (x == 1) Some("One") else None

    println("Map:")
    println(testSome.map(x => "Str %d".format(x)))
    println(testNone.map(x => "Str %d".format(x)))

    println("\nFlatMap:")
    println(testSome.flatMap(testFunction))
    println(testNone.flatMap(testFunction))

    println("\nGetOrElse:")
    println(testSome.getOrElse("None"))
    println(testNone.getOrElse("None"))

    println("\nOrElse:")
    println(testSome.orElse(Some(-1)))
    println(testNone.orElse(Some(-1)))

    println("\nFilter:")
    println(testSome.filter(x => x%2==0))
    println(testSome2.filter(x => x%2==0))
  }

  // Exercise 4.2
  def mean(ds: Seq[Double]): Option[Double] =
    ds.length match {
      case 0 => None
      case _ => Some(ds.sum / ds.length)
    }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m,2))))

  def ex_4_2(): Unit = {
    val testSeq = Seq(1.0, 2.0, 3.0) // Expected variance: 2/3
    println(variance(testSeq))
  }

  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.map(a_val => b.map(b_val => f(a_val, b_val))).getOrElse(None)

  def ex_4_3(): Unit = {
    println(map2(Some(1), Some("str %d"))((i,s) => s.format(i)))
    println(map2(None, Some("str %d"))((i,s) => s.format(i)))
    println(map2(Some(1), None)((i,s: String) => s.format(i)))
  }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case xo :: to => sequence(to).flatMap(t => xo.flatMap(x => Some(x :: t)))
  }

  def ex_4_4(): Unit = {
    val testList = List(Some(0), Some(1), Some(2))
    val testList2 = List(Some(1), None, Some(2))
    println(sequence(testList))
    println(sequence(testList2))
  }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t if f(h) == None => None
    case h :: t => traverse(t)(f).flatMap(xs => f(h).flatMap(x => Some(x::xs)))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)((x: Option[A]) => x)

  def ex_4_5(): Unit = {
    val testList = List(Some(0), Some(1), Some(2))
    val testList2 = List(Some(1), None, Some(2))
    println(sequence2(testList))
    println(sequence2(testList2))
  }

  // Exercise 4.6
  def ex_4_6(): Unit = {
    val testRight = Right(1): Either[String, Int]
    val testRight2 = Right(2): Either[String, Int]
    val testLeft = Left("Left value"): Either[String, Int]
    def testFunction(v: Int): Either[String, Int] =
      if (v==1) Left("This is a one")
      else Right(2)

    println("map:")
    println(testRight.map(x => x + 1))
    println(testLeft.map(x => x + 1))

    println("\nflatMap:")
    println(testRight.flatMap(x => testFunction(x)))
    println(testRight2.flatMap(x => testFunction(x)))
    println(testLeft.flatMap(x => testFunction(x)))

    println("\norElse:")
    println(testRight.orElse(Left("This is a left value!")))
    println(testLeft.orElse(Left("This is a left value!")))

    println("\nmap2:")
    println(testRight.map2(testRight2)((x, y) => x+y))
    println(testRight.map2(testLeft)((x, y) => x+y))
  }

  // Exercise 4.7
  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match  {
      case Nil => Right(Nil)
      case he :: te => he.map2(sequence2(te))((h, t) => h :: t)
    }

  def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h) match {
        case Left(e) => Left(e): Either[E, List[B]]
        case Right(v) => traverse2(t)(f).flatMap(l => Right(v :: l))
      }
    }

  def ex_4_7(): Unit = {
    val testList = List(Right(1), Right(2), Right(3))
    val testList2 = List(Right(1), Left("One"), Left("Two"))
    val testList3 = List(1, 2, 3)
    val testList4 = List(1, 2, 4)

    def testFunc(x: Int): Either[String, Int] =
      if (x == 3) Left("Three is in the list")
      else Right(x)

    def testFunc2(x: Int): Either[String, Int] =
      if (x >= 2) Left("Element present that is bigger than one")
      else Right(x)

    println(sequence2(testList))
    println(sequence2(testList2))
    println(traverse2(testList3)(testFunc))
    println(traverse2(testList3)(testFunc2))
    println(traverse2(testList4)(testFunc))
  }

  def main(args: Array[String]): Unit = {
    //ex_4_1()
    //ex_4_2()
    //ex_4_3()
    //ex_4_4()
    //ex_4_5()
    //ex_4_6()
    ex_4_7()
  }

  /* Exercise 4.8

    It would make most sense the change the way that map2 works such that Left values are not simply passed up but
    are added to a list of all of the Left values.

    A possibility might be to implement a sort of Either which has a list on the Left side. The list on the Left side
    could append any new exceptions then return this list at the completion of the function call. orElse would function
    about the same because it only operates on a single Either value, but traverse and sequence would behave differently
    in that they would not prematurely terminate on the encounter of the first Left value - rather iterating completely
    through whatever List is fed in.
   */
}

