case class Some[+A](val get: A) extends Option[A]
case object None extends Option[Nothing]

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

  def main(args: Array[String]): Unit = {
    ex_4_1()
  }
}

