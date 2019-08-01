trait List[+A] {
  def tail: List[A]
}
case object Nil extends List[Nothing] {
  override def tail = throw new RuntimeException
}
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](el: A*): List[A] = {
    if (el.isEmpty) Nil
    else Cons(el.head, apply(el.tail: _*))
  }

  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(a, b) => a + sum(b)
  }
}

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

@scala.annotation.tailrec
def drop[A](l: List[A], n: Int): List[A] = l match {
  case Cons(_, t) if n > 0 => drop(t, n - 1)
  case list if n == 0 => list
  case _ => Nil
}

drop(List(1, 2, 3, 4, 5), 3) == List(4, 5)
drop(Nil, 3) == Nil

@scala.annotation.tailrec
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case Cons(h, t) if f(h) => dropWhile(t)(f)
  case _ => l
}

dropWhile(List(1, 2, 3, 4, 5))(_ < 4) == List(4, 5)