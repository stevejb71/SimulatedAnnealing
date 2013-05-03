package simann

import org.specs2.mutable._
import scalaz.syntax.std.option._

class BoardSpec extends Specification {
  "A board can be printed" in {
    Board(List(2, 0, 1)).toString must be_=== ("""|..Q
                                                  |Q..
                                                  |.Q.""".stripMargin)

  }
}
