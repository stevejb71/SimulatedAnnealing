package simann

import org.specs2.mutable._

class EightQueensSpec extends Specification {
  "Initial board generation" in {
    EightQueens.initialBoard(4, new util.Random(50)) must be_=== (Board(1, 2, 0, 3))
  }

}
