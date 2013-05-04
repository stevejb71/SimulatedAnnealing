package simann

import org.specs2.mutable._
import util.Random
import scalaz.syntax.std.option._

class IntegrationSpec extends Specification {
  "Solve an 8 level board" in {
    produceSolutionAndCheck(8, 100, Board(3, 6, 4, 1, 5, 0, 2, 7))
  }

  "Solve a 10 level board" in {
    produceSolutionAndCheck(10, 100, Board(2, 7, 5, 0, 8, 1, 4, 6, 3, 9))
  }

  private def produceSolutionAndCheck(boardSize: Int, random: Int, expected: Board) {
    expected.countDiagonalConflicts must be_===(0)
    val solution = EightQueens.produceSolution(boardSize, new Random(random))
    solution must_== expected.some
  }
}
