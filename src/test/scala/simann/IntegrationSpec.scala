package simann

import org.specs2.mutable._
import scalaz.syntax.std.option._

class IntegrationSpec extends Specification {
  "Solve an 8 level board" in {
    produceSolutionAndCheck(8, 100, Board(3, 6, 4, 1, 5, 0, 2, 7))
  }

  "Solve a 10 level board" in {
    produceSolutionAndCheck(10, 100, Board(4, 2, 8, 5, 7, 1, 3, 0, 6, 9))
  }

  private def produceSolutionAndCheck(boardSize: Int, random: Int, expected: Board) {
    implicitly[Annealable[Board]].energy(expected) must be_===(0)
    val solution = EightQueens.produceSolution(boardSize).eval(Random(random))
    solution must_== expected.some
  }
}
