import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._

import scala.annotation.tailrec

object Code extends App {

  implicit def charToString(ch: Char) = ch.toString

  val a = "SEND"
  val b = "MORE"
  val c = "MONEY"

  val letters = List(a, b, c).mkString.toSet.toList.sorted
  val vars = letters.map(ch => int(Var(ch), 0, 9))

  add(Alldifferent(vars: _*))

  add(Var(a(0)) =/= 0)
  add(Var(b(0)) =/= 0)
  add(Var(c(0)) =/= 0)

  add(value(a) + value(b) === value(c))

  if (find) {
    for (sol <- solutions) {
      showSolution(sol)
      println()
    }
  } else {
    println("No solutions found!")
  }

  def value(word: String) = {
    @tailrec
    def go(chs: List[Char], mult: Int, acc: Term): Term =
      chs match {
        case ch :: tail =>
          go(tail, 10 * mult, acc + Var(ch.toString) * mult)
        case _ =>
          acc
      }

    go(word.toList.reverse, 1, Num(0))
  }

  def showSolution(sol: Solution) = {
    showWord(a, sol)
    showWord(b, sol)
    showWord(c, sol)
  }

  def showWord(word: String, sol: Solution) = {
    val vars = word.map(ch => Var(ch))
    val number = vars.map(sol.intValues(_)).mkString
    println(number.formatted("%5s"))
  }
}