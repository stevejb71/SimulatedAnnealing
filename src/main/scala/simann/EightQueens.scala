package simann

import scalaz.effect.{IO, SafeApp}
import scalaz.effect.IO.putStrLn
import scalaz.syntax.equal._
import scalaz.syntax.std.option._

case class Board(indicesAtRow: List[Int]) extends AnyVal {
  override def toString = {
    val blankRow = List.fill(indicesAtRow.size)('.')
    val rows = indicesAtRow.toList.map(blankRow.updated(_, 'Q'))
    rows.map(_.mkString).mkString("\n")
  }
}

object Board {
  def clean(size: Int) = Board((0 to size).toList)
}

object EightQueens extends SafeApp {
  private val initialTemperature = 30.0
  private val finalTemperature = 0.5
  private val alpha = 0.99
  private val stepsPerChange = 100

  override def runc: IO[Unit] = {
    val board = Board.clean(8)
    val solved = solveBoard(board)
    putStrLn(solved.toString)
  }

  private def solveBoard(board: Board) = ???
}
