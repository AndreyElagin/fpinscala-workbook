package part1

sealed trait List[+A] {
  def head: A
  def tail: List[A]
}

object List {
  case object Nil extends List[Nothing] {
    override def tail = throw new NoSuchElementException

    override def head = throw new NoSuchElementException
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  def apply[A](el: A*): List[A] = {
    if (el.isEmpty) Nil
    else Cons(el.head, apply(el.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def lengthR[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ => foldLeft(l.tail, Cons(l.head, Nil))((v, a: A) => Cons(a, v))
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRightViaFoldLeft(l, r)((a: A, value: List[A]) => Cons(a, value))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])(append)

  def plus1(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int])((v, list) => Cons(v + 1, list))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B])((v, list) => Cons(f(v), list))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A])((v, list) => if (f(v)) Cons(v, list) else list)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
}
