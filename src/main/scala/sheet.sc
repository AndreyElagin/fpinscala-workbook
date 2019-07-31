def fib(num: Int): Int = {
  if (num <= 0) num
  else fib(num - 1) + fib(num - 2)
}

def fib2(num: Int): Int = {
  def loop(count: Int, cur: Int, next: Int): Int = {
    if (count == 0) cur
    else loop(count - 1, next, cur + next)
  }
  loop(num - 1, 0, 1)
}

def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

trait List[+A]
case object Nil extends List[Nothing]
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