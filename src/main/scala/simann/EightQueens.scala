package simann

import scalaz.effect.{IO, SafeApp}
import scalaz.effect.IO.putStrLn
import scalaz.Show
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.stream._
import scalaz.syntax.equal._
import scalaz.syntax.foldable._
import scalaz.syntax.show._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import util.Random
import annotation.tailrec

case class Board(indicesAtRow: List[Int]) extends AnyVal {
  def size = indicesAtRow.size
  def swap(x: Int, y: Int) = Board(indicesAtRow.updated(x, indicesAtRow(y)).updated(y, indicesAtRow(x)))
  def countDiagonalConflicts = indicesAtRow.zipWithIndex.map{case (c, r) => countConflictsToWest(r, c, -1) + countConflictsToWest(r, c, 1)}.sum
  private def countConflictsToWest(row: Int, col: Int, dy: Int): Int = {
    val diagonalPositions = unfold((row, col)){case (r, c) => (r >= 0 && c >= 0 && r < size) ? ((r, c), (r + dy, c - 1)).some | None}.toList
    diagonalPositions.isEmpty ? 0 | diagonalPositions.tail.count{case (r, c) => hasQueen(r, c)}
  }
  private def hasQueen(row: Int, col: Int) = indicesAtRow(row) === col
}

object Board {
  def apply(qs: Int*): Board = Board(qs.toList)
  def clean(size: Int) = Board((0 until size).toList)
  implicit val showBoard = Show.shows((b: Board) => {
    val blankRow = List.fill(b.size)('.')
    val rows = b.indicesAtRow.toList.map(blankRow.updated(_, 'Q'))
    rows.map(_.mkString).mkString("\n")
  })
}

object EightQueens extends SafeApp {
  private val initialTemperature = 30.0
  private val finalTemperature = 0.5
  private val alpha = 0.99
  private val stepsPerChange = 100

  override def runc: IO[Unit] = {
    val random = new util.Random(100)
    val solved = produceSolution(8, random)
    solved.map(s => putStrLn(s.shows)).getOrElse(IO())
  }

  def produceSolution(boardSize: Int, random: Random) = {
    val board = Board.clean(boardSize)
    solveBoard(board, random)
  }

  def solveBoard(board: Board, random: Random): Option[Board] = {
    var bestBoard: Option[Board] = None
    var currentBoard = board
    var temperature = initialTemperature
    while(temperature > finalTemperature) {
      (0 until stepsPerChange).foreach {step =>
        val workingBoard = tweakBoard(currentBoard, random)
        val workingEnergy = workingBoard.countDiagonalConflicts
        val currentEnergy = currentBoard.countDiagonalConflicts
        val useNew = if(workingEnergy < currentEnergy) {
          true
        } else {
          val test = random.nextDouble()
          val delta = workingEnergy - currentEnergy
          val calc = math.exp(-delta / temperature)
          calc > test
        }
        if(useNew) {
          currentBoard = workingBoard
          if(currentBoard.countDiagonalConflicts < bestBoard.map(_.countDiagonalConflicts).getOrElse(10000000)) {
            bestBoard = currentBoard.some
          }
        }
        temperature *= alpha
      }
    }
    bestBoard
  }

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
