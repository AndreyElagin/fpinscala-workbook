package ds

import ds.Tree.{ Branch, Leaf }
import org.scalatest.{ Matchers, WordSpec }

class TreeTest extends WordSpec with Matchers {

  "A tree length" when {
    "if it only leaf" should {
      "be equal to 1" in {
        assert(Tree.size(Leaf(1)) == 1)
      }
    }
    "branch contains 2 elements" should {
      "be equal to 2" in {
        assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
      }
    }
    "tree contains 3 leafs in 2 branches" should {
      "be equal to 5" in {
        assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 5)
      }
    }
  }
}
