package simann

import org.specs2.mutable._
import scalaz.syntax.std.option._

class IntegrationSpec extends Specification {
  "Solve an 8 level board" in {
    produceSolutionAndCheck(8, 100, Board(4, 6, 1, 5, 2, 0, 3, 7))
  }

  "Solve a 10 level board" in {
    produceSolutionAndCheck(10, 100, Board(2, 4, 7, 1, 8, 6, 0, 3, 5, 9))
  }

  private def produceSolutionAndCheck(boardSize: Int, random: Int, expected: Board) {
    implicitly[Annealable[Board]].energy(expected) must be_===(0)
    val solution = EightQueens.produceSolution(boardSize).eval(Random(random))
    solution must_== expected.some
  }
}
