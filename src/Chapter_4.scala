case class Some[+A](val get: A) extends Option[A]
case object None extends Option[Nothing]

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
  //def sequence[A](a: List[Option[A]]): Option[List[A]] =

  def main(args: Array[String]): Unit = {
    //ex_4_1()
    //ex_4_2()
    //ex_4_3()
    val testList = List(1,2,3)
    println(testList.foldRight(0)(_+_))
  }
}

