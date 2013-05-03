package simann

import scalaz.effect.{IO, SafeApp}
import scalaz.effect.IO.putStrLn
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import util.Random
import annotation.tailrec

case class Board(indicesAtRow: List[Int]) extends AnyVal {
  def size = indicesAtRow.size
  def swap(x: Int, y: Int) = Board(indicesAtRow.updated(x, indicesAtRow(y)).updated(y, indicesAtRow(x)))
  override def toString = {
    val blankRow = List.fill(size)('.')
    val rows = indicesAtRow.toList.map(blankRow.updated(_, 'Q'))
    rows.map(_.mkString).mkString("\n")
  }
}

object Board {
  def apply(qs: Int*): Board = Board(qs.toList)
  def clean(size: Int) = Board((0 until size).toList)
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

  private[simann] def initialBoard(size: Int, random: Random): Board = (1 to size).foldLeft(Board.clean(size))((b, _) => tweakBoard(b, random))

  @tailrec
  private def tweakBoard(board: Board, random: Random): Board = {
    // TODO: not functional
    val x = random.nextInt(board.size - 1)
    val y = random.nextInt(board.size - 1)
    if(x === y) {
      tweakBoard(board, random)
    } else {
      board.swap(x, y)
    }
  }
}
