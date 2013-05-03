package simann

import org.specs2.mutable._
import scalaz.syntax.show._

class BoardSpec extends Specification {
  "A board can be printed" in {
    Board(2, 0, 1).shows must be_=== ("""|..Q
                                         |Q..
                                         |.Q.""".stripMargin)

  }

  "Queens can be swapped" in {
    Board(2, 0, 1).swap(0, 2) must be_=== (Board(1, 0, 2))
  }

  "Conflicts can be detected" in {
    Board(1, 2, 3, 0).countDiagonalConflicts must be_=== (4)
  }

  "No conflict board" in {
    Board(3, 6, 2, 5, 1, 4, 0).countDiagonalConflicts must be_=== (0)
  }
}
