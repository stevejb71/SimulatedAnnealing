package simann

import scalaz.effect.{IO, SafeApp}
import scalaz.syntax.equal._
import scalaz.syntax.std.option._

case class Board(indicesAtRow: List[Option[Int]]) extends AnyVal {
  override def toString = {
    val blankRow = List.fill(indicesAtRow.size)('.')
    val rows = indicesAtRow.toList.map(_.map(blankRow.updated(_, 'Q')).getOrElse(blankRow))
    rows.map(_.mkString).mkString("\n")
  }
}
object Board {
  def clean(size: Int) = Board(List.fill(size)(None))
}

object EightQueens extends SafeApp {
  override def runc: IO[Unit] = IO {
    val board = Board.clean(8)
  }
}
