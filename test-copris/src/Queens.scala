import jp.kobe_u.copris.dsl._
import jp.kobe_u.copris.{Abs, Alldifferent, Solution}

object Queens extends App {

  val N = 8

  val qs = (1 to N).map(i => int('q (i), 0, N - 1))

  add(Alldifferent(qs: _*))

  for (d <- 1 to N - 1) {
    for (col <- 0 until N - d) {
      add(Abs(qs(col) - qs(col + d)) =/= d)
    }
  }

  if (find) {
    var count = 0;
    val sols = solutions.toList.sortBy(rank)
    for (sol <- sols) {
      show(sol)
      println()
      count += 1
    }
    println(s"Found ${count} solutions.")
  } else {
    println("No solutions found!")
  }

  def rank(sol: Solution) = {
    qs.map(q => sol.intValues(q)).toList.toString
  }

  def show(sol: Solution) = {
    for (row <- 0 until N) {
      for (col <- 0 until N) {
        print(if (sol.intValues(qs(col)) == row) "Q" else "-")
        print(" ")
      }
      println()
    }
  }
}
