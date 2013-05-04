package simann

import org.specs2.mutable._
import util.Random
import scalaz.syntax.std.option._

class IntegrationSpec extends Specification {
  "Solve an 8 level board" in {
    produceSolutionAndCheck(8, 100, Board(3, 6, 4, 1, 5, 0, 2, 7))
  }

  "Solve a 10 level board" in {
    produceSolutionAndCheck(10, 100, Board(3, 5, 0, 7, 1, 6, 8, 2, 4, 9))
  }

  private def produceSolutionAndCheck(boardSize: Int, random: Int, expected: Board) {
    val solution = EightQueens.produceSolution(boardSize, new Random(random))
    println(solution)
    solution must_== expected.some
  }
}
