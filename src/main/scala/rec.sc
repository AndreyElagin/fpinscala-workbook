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