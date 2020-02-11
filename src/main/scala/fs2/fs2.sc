import java.util.concurrent.atomic.AtomicLong

import fs2.Stream
import cats.effect.IO
import fs2.Chunk

Stream
  .emits(List(1, 2, 3))
  .map(_ * 666)
  .intersperse("vasian")
  .toVector

val eff = Stream.eval(IO { println("BEING RUN!!"); 1 + 1 })

eff.compile.toVector.unsafeRunSync

val s1c = Stream
  .chunk(Chunk.doubles(Array(1.0, 2.0, 3.0)))
  .mapChunks(_.toDoubles)
  .toVector

val appendEx1 = Stream(1, 2, 3) ++ Stream.emit(42)
val appendEx2 = Stream(1, 2, 3) ++ Stream.eval(IO.pure(4))

val some = appendEx1 ++ appendEx2
some.compile.toVector.unsafeRunSync

val count = new AtomicLong(0)
val acquire = IO { println("incremented: " + count.incrementAndGet); () }
val release = IO { println("decremented: " + count.decrementAndGet); () }
val err = Stream.raiseError[IO](new Exception("oh noes!"))

Stream
  .bracket(acquire)(_ => release)
  .flatMap(_ => Stream(1, 2, 3) ++ err)
  .compile
  .drain
  .unsafeRunSync

println(count.get)
