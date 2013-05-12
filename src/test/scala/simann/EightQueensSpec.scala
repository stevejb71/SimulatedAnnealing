package simann

import org.specs2.mutable._

class EightQueensSpec extends Specification {
  "Initial board generation" in {
    EightQueens.initialBoard(4).eval(Random(50)) must be_=== (Board(0, 2, 1, 3))
  }

}
