import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._

object Sudoku extends App {
  val N = 9 // the grid size
  val M = 3 // the sub-grid size

//  val puzzle = parse(
//    """
//    6 . . | . . 5 | . 3 .
//    . 1 . | 4 2 . | . . 6
//    . . 4 | . . . | . . .
//    ------+-------+------
//    . . 5 | . 7 . | . 9 .
//    . 2 . | . . 9 | . 8 .
//    . . 1 | . . 6 | . . .
//    ------+-------+------
//    4 7 . | . . . | . . .
//    . 3 . | . . . | 9 . 1
//    . . . | . . 8 | . . 3
//  """)

  val puzzle = parse(
    """
    1 . . | . . . | . . .
    . 2 . | . . . | . . .
    . . 3 | . . . | . . .
    ------+-------+------
    . . . | 4 . . | . . .
    . . . | . 5 . | . . .
    . . . | . . 6 | . . .
    ------+-------+------
    . . . | . . . | 7 . .
    . . . | . . . | . 8 .
    . . . | . . . | . . 9
  """)

  solve(puzzle)

  def solve(puzzle: Seq[Seq[Int]]) = {
    // Create the variables.
    for (row <- 0 until N; col <- 0 until N)
      int('cell (row, col), 1, N)

    // Each row must contain different numbers.
    for (row <- 0 until N)
      add(Alldifferent((0 until N).map('cell (row, _))))

    // Each column must contain different numbers.
    for (col <- 0 until N)
      add(Alldifferent((0 until N).map('cell (_, col))))

    // Each 3x3 grid must contain different numbers.
    for (row <- 0 until N by M; col <- 0 until N by M)
      add(Alldifferent(for (drow <- 0 until M; dcol <- 0 until M) yield 'cell (row + drow, col + dcol)))

    // Copy the initial values.
    for (row <- 0 until N; col <- 0 until N; if puzzle(row)(col) > 0)
      add('cell (row, col) === puzzle(row)(col))

    if (find) {
      for (sol <- solutions) {
        showSolution(sol)
        println()
      }
    } else {
      println("No solution!")
    }
  }

  def parse(text: String) =
    text
      .split("\n")
      .map(_.strip)
      .filter(_.size > 0)
      .filter(!_.startsWith("------"))
      .map(_.filter(ch => ch.isDigit || ch == '.'))
      .map(_.replace(".", "0"))
      .map(_.map(_ - '0'))

  def showSolution(sol: Solution) = {
    def formatRow(row: Int) =
      (0 until N)
        .map(col => sol('cell (row, col)))
        .grouped(M)
        .map(_ mkString " ")
        .mkString(" | ")

    val rows = (0 until N).map(formatRow)
    val hdiv = (0 until M).map(i => "-" * (2 * M - 1)).mkString("\n", "-+-", "\n")

    println(rows.grouped(M).map(_ mkString "\n").mkString(hdiv))
  }
}
