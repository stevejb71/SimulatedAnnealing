package simann

import org.specs2.mutable._

class RandomSpec extends Specification {
  "can use Random as a monad" in {
    val added = for {
      r1 <- FRandom.nextInt
      r2 <- FRandom.nextInt
    } yield (r1 + r2)
    added.eval(FRandom(10)) must be_=== (added.eval(FRandom(10)))
    added.eval(FRandom(10)) must not be_=== (added.eval(FRandom(11)))
  }
}
