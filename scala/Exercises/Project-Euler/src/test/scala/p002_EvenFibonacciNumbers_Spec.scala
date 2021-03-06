/*
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

        1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
 */

import org.scalatest.FreeSpec

class p002_EvenFibonacciNumbers_Spec extends FreeSpec {

  lazy val fibs: Stream[Long] = 1L #:: 2L #:: fibs.zip(fibs.tail).map { case (p, q) => p + q }

  "The fibs stream works" in {
    assert(fibs(0) == 1)
    assert(fibs(1) == 2)
    assert(fibs(2) == 3)
    assert(fibs(3) == 5)
    assert(fibs(4) == 8)
    assert(fibs(5) == 13)
  }

  "The solution is correct" in {
    val ans1 = fibs.takeWhile(_ <= 4000000L).filter(_ % 2 == 0).sum
    assert(ans1 == 4613732)

    val ans2 = fibs.filter(_ % 2 == 0).takeWhile(_ <= 4000000L).sum
    assert(ans2 == 4613732)
  }
}
