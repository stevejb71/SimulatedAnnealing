package simann

import org.specs2.mutable._

class RandomSpec extends Specification {
  "can use Random as a monad" in {
    val added = for {
      r1 <- Random.nextInt
      r2 <- Random.nextInt
    } yield (r1 + r2)
    added.eval(Random(10)) must be_=== (added.eval(Random(10)))
    added.eval(Random(10)) must not be_=== (added.eval(Random(11)))
  }
}
