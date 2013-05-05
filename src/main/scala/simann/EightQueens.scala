package simann

import scalaz.effect.{IO, SafeApp}
import scalaz.effect.IO.putStrLn
import scalaz._, Scalaz._
import util.Random

case class Board(indicesAtRow: List[Int]) extends AnyVal {
  def size = indicesAtRow.size
  def swap(x: Int, y: Int) = Board(indicesAtRow.updated(x, indicesAtRow(y)).updated(y, indicesAtRow(x)))
  def countDiagonalConflicts = indicesAtRow.zipWithIndex.map { case (c, r) => countConflictsToWest(r, c, -1) + countConflictsToWest(r, c, 1) }.sum
  private def countConflictsToWest(row: Int, col: Int, dy: Int): Int = {
    val diagonalPositions = unfold((row, col)) {
      case (r, c) => (r >= 0 && c >= 0 && r < size) ?? ((r, c), (r + dy, c - 1)).some
    }.toList
    diagonalPositions.isEmpty ? 0 | diagonalPositions.tail.count {
      case (r, c) => hasQueen(r, c)
    }
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
  override def runc: IO[Unit] = {
    val random = new util.Random(100)
    val solved = produceSolution(8, random)
    solved.map(s => putStrLn(s.shows)).getOrElse(IO())
  }

  def produceSolution(boardSize: Int, random: Random) = {
    val board = initialBoard(boardSize, random)
    Annealing.anneal(board, random, AnnealingConfig(30.0, 0.5, 0.99, 100))
  }

  implicit val boardIsAnnealable = new Annealable[Board] {
    def tweak(board: Board, nextInt: Int => Int): Board = {
      val x = nextInt(board.size - 1)
      val y = nextInt(board.size - 1)
      if (x === y) {
        tweak(board, nextInt)
      } else {
        board.swap(x, y)
      }
    }
    def energy(b: Board): Double = b.countDiagonalConflicts
  }

  private[simann] def initialBoard(size: Int, random: Random): Board = (1 to size).foldLeft(Board.clean(size))((b, _) => boardIsAnnealable.tweak(b, random.nextInt _))
}
