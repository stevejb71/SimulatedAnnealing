package simann

import org.specs2.mutable._

class BoardSpec extends Specification {
  "A board can be printed" in {
    Board(2, 0, 1).toString must be_=== ("""|..Q
                                            |Q..
                                            |.Q.""".stripMargin)

  }

  "Queens can be swapped" in {
    Board(2, 0, 1).swap(0, 2) must be_=== (Board(1, 0, 2))
  }
}
