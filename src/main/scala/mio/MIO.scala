package mio

class MIO[A](f: (Either[Throwable, A] => Unit) => Unit) {

  def map[B](m: A => B): MIO[B] = new MIO[B](
    cb =>
      f {
        case Right(v) => cb(Right(m(v)))
        case l @ Left(_) => l
      }
  )

  def flatMap[B](fm: A => MIO[B]): MIO[B] = new MIO[B](
    cb => () // TODO:
  )

  // for debug only
  override def toString: String = {
    var ref = ""
    f {
      case Left(value) => ref = "wrong"
      case Right(value) => ref = value.toString
    }
    ref
  }
}

object MIO {
  def pure[A](a: A): MIO[A] = new MIO[A](cb => cb(Right(a)))
}
