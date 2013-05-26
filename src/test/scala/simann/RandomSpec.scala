package simann

import org.specs2.mutable._
import scalaz._, Scalaz._
import simann.Namespace._

class RandomSpec extends Specification {
  val seed = 55
  val count = 100
  def scalaRandomRs[A](lo: A, hi: A)(implicit R: Namespace.Random[A]) = {
    val stdGen = StdGen.scramble(seed)
    (0 until count).toList.traverseU(_ => State(R.randomR(lo, hi) _)).eval(stdGen)
  }
  def javaRandoms[A](f: java.util.Random => A) = {
    val javaRandom = new java.util.Random(seed)
    (0 until count).map(_ => f(javaRandom)).toList
  }

  "Random int the same as Java" in {
    scalaRandomRs(0, 1000) must be_===(javaRandoms(_.nextInt(1000)))
  }

  "Random boolean the same as Java" in {
    scalaRandomRs(false, true) must be_===(javaRandoms(_.nextBoolean))
  }

  "Random double the same as Java" in {
    scalaRandomRs(0.0, 1.0) must be_===(javaRandoms(_.nextDouble))
  }
}
