
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
}

object Chapter_6 {
  // Exercise 6.1
  def randomInt(rng: RNG): (Int, RNG) = {
    val (i1, next_rng) = rng.nextInt

    if (i1 == Int.MinValue) randomInt(next_rng)
    else (math.abs(i1), next_rng)
  }

  def ex_6_1(): Unit = {
    val rng_1 = SimpleRNG(0)
    val (_, rng_2) = randomInt(rng_1)
    val (_, rng_3) = randomInt(rng_2)

    println("IntMax: %d".format(Int.MaxValue))
    println("IntMin: %d".format(Int.MinValue))
    println(randomInt(rng_1))
    println(randomInt(rng_2))
    println(randomInt(rng_3))
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i1, next_rng) = randomInt(rng)

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

  def main(args: Array[String]): Unit = {
    //ex_6_1()
    ex_6_2()
  }
}
