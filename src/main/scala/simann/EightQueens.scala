package simann

import scalaz.effect.{IO, SafeApp}
import scalaz.effect.IO.putStrLn
import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.std.list._
import scalaz.std.anyVal._
import util.Random
import annotation.tailrec
import scalaz.std.stream._

case class Board(indicesAtRow: List[Int]) extends AnyVal {
  def size = indicesAtRow.size
  def swap(x: Int, y: Int) = Board(indicesAtRow.updated(x, indicesAtRow(y)).updated(y, indicesAtRow(x)))
  def countDiagonalConflicts = indicesAtRow.zipWithIndex.map{case (r, c) => countConflictsToNorthWest(r, c) + countConflictsToSouthWest(r, c)}.sum
  private def countConflictsToNorthWest(row: Int, col: Int) = countConflictsToWest(row, col, -1)
  private def countConflictsToSouthWest(row: Int, col: Int) = countConflictsToWest(row, col, 1)
  private def countConflictsToWest(row: Int, col: Int, dy: Int): Int = {
    val diagonalPositions = unfold((row, col)){case (r, c) => (r >= 0 && c >= 0 && r < size) ? ((r, c), (r + dy, c - 1)).some | None}.toList
    diagonalPositions.isEmpty ? 0 | diagonalPositions.tail.count{case (r, c) => hasQueen(r, c)}
  }
  private def hasQueen(row: Int, col: Int) = indicesAtRow(row) === col
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

  private[simann] def computeEnergy(board: Board): Int = ???

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
