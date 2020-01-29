package part1

import part1.List.{append, filter, filterFlatMap, flatMap, map, plus1, product, reverse, sum}
import part1.List.Nil
import org.scalatest.{FlatSpec, Matchers, WordSpec}

class ListTest extends WordSpec with Matchers {

  "A list length" when {
    "list is empty" should {
      "be equal to 0" in {
        assert(List.length(Nil) == 0)
      }
    }
    "list contains 5 elements" should {
      "be equal to 5" in {
        assert(List.length(List(1, 2, 3, 4, 5)) == 5)
      }
    }
  }

  "A List sum" when {
    "list is empty" should {
      "be equal to 0" in {
        assert(sum(Nil) == 0)
      }
    }
    "list contains elements: 1, 2, 3" should {
      "be equal to 6" in {
        assert(sum(List(1, 2, 3)) == 6)
      }
    }
  }

  "A List product" when {
    "list is empty" should {
      "be equal to 0.0" in {
        assert(product(Nil) == 1.0)
      }
    }
    "list contains elements: 1, 2, 4" should {
      "be equal to 8" in {
        assert(product(List(1, 2, 4)) == 8)
      }
    }
    "list contains at least one zero" should {
      "be equal to 0" in {
        assert(product(List(1, 2, 4, 0)) == 0.0)
      }
    }
  }

  "A list reverse" when {
    "list contains elements: 1, 2, 3" should {
      "return List(3, 2, 1)" in {
        assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
      }
    }
    "list is empty" should {
      "return Nil" in {
        assert(reverse(Nil) == Nil)
      }
    }
  }

  "A list append" when {
    "try to add element" should {
      "return new list with this element in last posititon" in {
        assert(append(List(1, 2, 3), List(666)) == List(1, 2, 3, 666))
      }
    }
  }

  "A list plus1" when {
    "invoked on List(1, 2, 3)" should {
      "return new List(2, 3, 4)" in {
        assert(plus1(List(1, 2, 3)) == List(2, 3, 4))
      }
    }
  }

  "A list map" when {
    "applied to List(1, 2, 3) with mapping function (_ + 666)" should {
      "return new List(667, 668, 669)" in {
        assert(map(List(1, 2, 3))(_ + 666) == List(667, 668, 669))
      }
    }
  }

  "A list filter" when {
    "applied to List(1, 2, 3) with predicate (_ < 3)" should {
      "return new List(1, 2)" in {
        assert(filter(List(1, 2, 3))(_ < 3) == List(1, 2))
      }
    }
  }

  "A list flatMap" when {
    "applied to List(1, 2, 3) with mapping function i => List(i,i)" should {
      "return new List(1,1,2,2,3,3)" in {
        assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
      }
    }
  }

  "A list filterFlatMap" when {
    "applied to List(1, 2, 3) with with predicate (_ < 3)" should {
      "return new List(1, 2)" in {
        assert(filterFlatMap(List(1, 2, 3))(_ < 3) == List(1, 2))
      }
    }
  }
}
