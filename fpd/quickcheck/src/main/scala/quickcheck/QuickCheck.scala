package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = frequency(
    (1, const(empty)),
    (100, for {
      newElem <- arbitrary[A]
      h <- genHeap
    } yield insert(newElem, h))
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == ord.min(a, b)
  }

  property("insert&delete1") = forAll { (a: A) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("insert&delete2") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    val oneDeleted = deleteMin(h)
    findMin(h) == ord.min(a, b) && findMin(oneDeleted) == ord.max(a, b)
  }

  property("deleteN") = forAll(listOfN(100, arbitrary[A])) { (elems: List[A]) =>
    val h = elems.foldLeft(empty)((h, newElem) => insert(newElem, h))
    val deleted = (1 to elems.length).foldLeft(h)((h, _) => deleteMin(h))
    isEmpty(deleted)
  }

  property("sort") = forAll { (h: H) =>
    def isCorrectSortedElems(h: H, prev: A): Boolean =
      if isEmpty(h) then true
      else ord.lteq(prev, findMin(h)) && isCorrectSortedElems(deleteMin(h), findMin(h))

    if isEmpty(h) then true
    else isCorrectSortedElems(deleteMin(h),findMin(h))
  }

  property("sortFromElems") = forAll(listOfN(100, arbitrary[A])) { (elems: List[A]) =>
    val h = elems.foldLeft(empty)((h, newElem) => insert(newElem, h))
    def sortedElems(h: H): List[A] =
      if isEmpty(h) then List()
      else findMin(h) :: sortedElems(deleteMin(h))

    elems.sorted(ord).sameElements(sortedElems(h))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)

    if isEmpty(h1) && isEmpty(h2) then true
    else if isEmpty(h1) then findMin(melded) == findMin(h2)
    else if isEmpty(h2) then findMin(melded) == findMin(h1)
    else findMin(melded) == ord.min(findMin(h1), findMin(h2))

  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

