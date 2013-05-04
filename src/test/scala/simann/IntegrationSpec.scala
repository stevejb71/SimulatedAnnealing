package simann

import org.specs2.mutable._
import util.Random
import scalaz.syntax.std.option._

class IntegrationSpec extends Specification {
  "Solve an 8 level board" in {
    EightQueens.produceSolution(8, new Random(100)) must be_=== (Board(List(3, 6, 4, 1, 5, 0, 2, 7)).some)
  }

}
