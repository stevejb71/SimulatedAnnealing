package simann

import org.specs2.mutable._
import util.Random
import scalaz.syntax.std.option._

class IntegrationSpec extends Specification {
  "Solve an 8 level board" in {
    produceSolutionAndCheck(8, 100, Board(5, 3, 6, 0, 2, 4, 1, 7))
  }

  "Solve a 10 level board" in {
    produceSolutionAndCheck(10, 100, Board(7, 3, 8, 6, 2, 0, 5, 1, 4, 9))
  }

  private def produceSolutionAndCheck(boardSize: Int, random: Int, expected: Board) {
    expected.countDiagonalConflicts must be_===(0)
    val solution = EightQueens.produceSolution(boardSize, new Random(random))
    solution must_== expected.some
  }
}
