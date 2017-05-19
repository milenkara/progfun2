package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h  <- frequency((1, empty), (9, genHeap))
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("ins-del") = forAll { (a: A) =>
   isEmpty(deleteMin(insert(a, empty))) == true
  }

  property("meld-min") = forAll{ (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)

    Seq(min1, min2).contains(findMin(meld(h1, h2)))
  }

  property("insert two values in empty") = forAll { (a1: A, a2: A) =>
    findMin(insert(a2,insert(a1,empty))) == (if(a1<a2) a1 else a2)
  }

  property("sorted") = forAll { (h: H) =>

    def allElements(b: H): List[A] = {
      if(isEmpty(b))
        Nil
      else{
        findMin(b) :: allElements(deleteMin(b))
      }
    }

    val a = allElements(h)
    allElements(h) == allElements(h).sorted
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
