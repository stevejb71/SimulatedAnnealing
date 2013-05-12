package simann

import scalaz.effect.{IO, SafeApp}
import scalaz.effect.IO.putStrLn
import scalaz._, Scalaz._

case class Board(indicesAtRow: List[Int]) extends AnyVal {
  def size = indicesAtRow.size
  def swap(x: Int, y: Int) = Board(indicesAtRow.updated(x, indicesAtRow(y)).updated(y, indicesAtRow(x)))
  def hasQueen(row: Int, col: Int) = indicesAtRow(row) === col
}

object Board {
  def apply(qs: Int*): Board = Board(qs.toList)
  def clean(size: Int) = Board((0 until size).toList)

  implicit val boardHasStringRep = Show.shows((b: Board) => {
    val blankRow = List.fill(b.size)('.')
    val rows = b.indicesAtRow.toList.map(blankRow.updated(_, 'Q'))
    rows.map(_.mkString).mkString("\n")
  })

  implicit val boardIsAnnealable = new Annealable[Board] {
    def heat(board: Board): State[Random, Board] = for {
      x <- Random.nextInt(board.size - 1)
      y <- Random.nextInt(board.size - 1)
      heated <- (x === y) ? heat(board) | board.swap(x, y).pure[({type M[B] = State[Random, B]})#M]
    } yield heated

    def energy(b: Board): Double = {
      def countConflictsToWest(row: Int, col: Int, dy: Int): Int = {
        val diagonalPositions = unfold((row, col)) {
          case (r, c) => (r >= 0 && c >= 0 && r < b.size) ?? ((r, c), (r + dy, c - 1)).some
        }.toList
        diagonalPositions.isEmpty ? 0 | diagonalPositions.tail.count {
          case (r, c) => b.hasQueen(r, c)
        }
      }
      b.indicesAtRow.zipWithIndex.map {
        case (c, r) => countConflictsToWest(r, c, -1) + countConflictsToWest(r, c, 1)
      }.sum
    }
  }
}

object EightQueens extends SafeApp {
  override def runc: IO[Unit] = RandomIO.random.flatMap {random =>
    val result = produceSolution(8).eval(random).map(_.shows).getOrElse("No solution found")
    putStrLn(result)
  }

  def produceSolution(boardSize: Int): State[Random, Option[Board]] =
    initialBoard(boardSize).flatMap(board => Annealing.anneal(board, AnnealingConfig(Temperature(30.0), Temperature(0.5), 0.99, 100)))

  private[simann] def initialBoard(size: Int): State[Random, Board] = {
    (1 to size).toList.foldLeftM[({type M[B] = State[Random, B]})#M, Board](Board.clean(size))((b, _) => {
      val annealable = implicitly[Annealable[Board]]
      annealable.heat(b)
    })
  }
}
