trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
}

object Chapter_6 {
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, next_rng) = rng.nextInt

    if (i1 == Int.MinValue) nonNegativeInt(next_rng)
    else (math.abs(i1), next_rng)
  }

  def ex_6_1(): Unit = {
    val rng_1 = SimpleRNG(0)
    val (_, rng_2) = nonNegativeInt(rng_1)
    val (_, rng_3) = nonNegativeInt(rng_2)

    println("IntMax: %d".format(Int.MaxValue))
    println("IntMin: %d".format(Int.MinValue))
    println(nonNegativeInt(rng_1))
    println(nonNegativeInt(rng_2))
    println(nonNegativeInt(rng_3))
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i1, next_rng) = nonNegativeInt(rng)

    if (i1 == Int.MaxValue) double(next_rng)
    else (i1.toDouble / Int.MaxValue, next_rng)
  }

  def ex_6_2(): Unit = {
    val rng_1 = SimpleRNG(0)
    val (_, rng_2) = double(rng_1)
    val (_, rng_3) = double(rng_2)

    println(double(rng_1))
    println(double(rng_2))
    println(double(rng_3))
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng_1) = nonNegativeInt(rng)
    val (d, rng_2) = double(rng_1)

    ((i, d), rng_2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), next_rng) = intDouble(rng)

    ((d, i), next_rng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng_1) = double(rng)
    val (d2, rng_2) = double(rng_1)
    val (d3, rng_3) = double(rng_2)

    ((d1, d2, d3), rng_3)
  }

  def ex_6_3(): Unit = {
    val rng = SimpleRNG(0)

    println(intDouble(rng))
    println(doubleInt(rng))
    println(double3(rng))
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (i, rng_1) = nonNegativeInt(rng)

    if (count == 0) (Nil, rng)
    else if (count == 1) (List(i), rng_1)
    else {
      val (t, rng_2) = ints(count - 1)(rng_1)
      (i::t, rng_2)
    }
  }

  def ex_6_4(): Unit = {
    val rng = SimpleRNG(0)

    println(ints(5)(rng))
  }

  // Exercise 6.5
  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def double_2: Rand[Double] =
    map(nonNegativeInt)(i => i match {
      case Int.MaxValue => (i - 1).toDouble / Int.MaxValue
      case _ => i.toDouble / Int.MaxValue
    })

  def ex_6_5(): Unit = {
    val rng = SimpleRNG(11)
    println(double_2(rng))
  }

  // Exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng_a) = ra(rng)
      val (b, rng_b) = rb(rng_a)

      (f(a, b), rng_b)
    }
  }

  def ex_6_6(): Unit = {
    val rng = SimpleRNG(1)

    println(map2(nonNegativeInt, double_2)((i, d) => (i, d))(rng))
  }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case h::t => rng => map2(h, sequence(t))((a, la) => a::la)(rng)
    case Nil => rng => (List[A](), rng)
  }

  def ints2(n: Int): Rand[List[Int]] = {
    val r_list: List[Rand[Int]] = List.fill(n)(nonNegativeInt)

    sequence(r_list)
  }

  def ex_6_7(): Unit = {
    val rng = SimpleRNG(1)

    println(ints2(5)(rng))
  }

  def main(args: Array[String]): Unit = {
    //ex_6_1()
    //ex_6_2()
    //ex_6_3()
    //ex_6_4()
    //ex_6_5()
    //ex_6_6()
    ex_6_7()
  }
}
