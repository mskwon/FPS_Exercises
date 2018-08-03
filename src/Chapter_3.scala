object Chapter_3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

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
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    def shiftedFunction(acc: B, node: A): B =
      f(node, acc)

    foldLeft(reverse(as), z)(shiftedFunction)
  }

  def ex_3_13(): Unit = {
    def outputMessage(foldtype: String, result: Int): Unit = {
      var msg = "The result of %s is %d"
      println(msg.format(foldtype, result))
    }

    var testList = List(1,2,3,4)

    outputMessage("foldLeft", foldLeft(testList, 0)(_ - _))
    outputMessage("foldRight version 1", foldRight(testList, 0)(_ - _))
    outputMessage("foldRight version 2", foldRight2(testList, 0)(_ - _))
  }

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)(Cons(_, _))

  def ex_3_14(): Unit = {
      var testList1 = List(1, 2, 3)
      var testList2 = List(4, 5, 6)
      println(append2(testList1, testList2))
  }

  // Exercise 3.15
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def flatten2D[A](l: List[List[A]]): List[A] =
    foldRight2(l, Nil: List[A])((list, flat) => foldRight2(list, flat)((x, xs) => Cons(x, xs)))

  def ex_3_15(): Unit = {
    var testList = List(List(1,2,3),List(4,5,6),List(7,8,9))
    println(flatten2D(testList))
  }

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] =
    foldRight2(l, Nil: List[Int])((x, xs) => Cons(x+1, xs))

  def ex_3_16(): Unit = {
    var testList = List(1,2,3)
    println(addOne(testList))
  }

  // Exerise 3.17
  def doubleToString(d: List[Double]): List[String] =
    foldRight(d, Nil: List[String])((d, ss) => Cons(d.toString(), ss))

  def ex_3_17(): Unit = {
    var testList = List(1.0, 2.0, 3.0)
    println(doubleToString(testList))
  }

  // Exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def ex_3_18(): Unit = {
    var testList = List(1,2,3)
    println(map(testList)(x => x+1))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(_, xs) => filter(xs)(f)
    }

  def ex_3_19(): Unit = {
    var testList = List(1,2,3,4)
    println(filter(testList)(x => x%2==0))
  }

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => append2(f(x),flatMap(xs)(f))
    }

  def ex_3_20(): Unit = {
    var testList = List(1,2,3)
    println(flatMap(testList)(i => List(i,i)))
  }

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if (f(x)) List(x) else Nil)
  }

  def ex_3_21(): Unit = {
    var testList = List(1,2,3,4)
    println(filter2(testList)(x => x%2==0))
  }

  // Exercise 3.22
  def addTogether(la: List[Int], lb: List[Int]): List[Int] =
    la match {
      case Nil => lb match {
        case Nil => Nil
        case _ => lb
      }
      case Cons(x, xs) => lb match {
        case Nil => la
        case Cons(y, ys) => Cons(x + y, addTogether(xs, ys))
      }
  }

  def ex_3_22(): Unit = {
    var testListA = List(1,2,3)
    var testListB = List(10,10,10,4)
    println(addTogether(testListA, testListB))
  }

  // Exercise 3.23
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] =
    as match {
      case Nil => bs match {
        case Nil => Nil
        case _ => bs
      }
      case Cons(x, xs) => bs match {
        case Nil => Cons(x, xs)
        case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
      }
    }

  def ex_3_23(): Unit = {
    var testListA = List(1,2,3)
    var testListB = List(10,10,10)
    println(zipWith(testListA, testListB)((x, y) => x * y))
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sub match {
      case Nil => true
      case Cons(x, xs) => sup match {
        case Nil => false
        case Cons(y, ys) if x == y => hasSubsequence(ys, xs)||(hasSubsequence(ys, sub))
        case Cons(_, ys) => hasSubsequence(ys, sub)
      }
    }

  def ex_3_24(): Unit = {
    var testList = List(1,2,2,3,4)
    var subListA = List(1,1)
    var subListB = List(2,3,4)
    var subListC = List(1,2)

    println(hasSubsequence(testList, subListA))
    println(hasSubsequence(testList, subListB))
    println(hasSubsequence(testList, subListC))
  }

  // Exercise 3.25
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](root: Tree[A]): Int =
    root match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

  def ex_3_25(): Unit = {
    var testTree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    println(size(testTree))
  }

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
    //ex_3_12()
    //ex_3_13()
    //ex_3_14()
    //ex_3_15()
    //ex_3_16()
    //ex_3_17()
    //ex_3_18()
    //ex_3_19()
    //ex_3_20()
    //ex_3_21()
    //ex_3_22()
    //ex_3_23()
    //ex_3_24()
    ex_3_25()
  }
}


