package part3

import org.scalatest.{ FreeSpec, Matchers }
import part3.Monoids.{ Monoid, foldMapV }

class MonoidTest extends FreeSpec with Matchers {

  private def stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero: String = ""
  }

  val strSeq = IndexedSeq("Satan", "My", "Friend", "Not", "Master")
  val friend = "SatanMyFriendNotMaster"

  val numSeq = IndexedSeq(42, 666, 23, 28, 53)
  val merged = "42666232853"

  s"should concat $strSeq to $friend" in {
    foldMapV(strSeq, stringMonoid)(i => i) shouldBe friend
  }

  s"should map and concat $numSeq to $merged" in {
    foldMapV(numSeq, stringMonoid)(_.toString) shouldBe merged
  }
}
