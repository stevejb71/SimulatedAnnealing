package simann

import org.specs2.mutable._
import scalaz._, Scalaz._
import simann.Namespace._

class RandomSpec extends Specification {
  "Random int the same as Java" in {
    val seed = 55
    val count = 100

    val scalaRandom = randomInt.randomR(0, 1000) _
    var stdGen: RandomGen = StdGen.scramble(seed)
    val scalaValues = (0 until count).map {_ =>
      val state: (Int, RandomGen) = scalaRandom(stdGen)
      stdGen = state._2
      state._1
    }.toList

    val javaRandom = new java.util.Random(seed)
    val javaValues = (0 until count).map {_ =>
      javaRandom.nextInt(1000)
    }.toList

    scalaValues must be_===(javaValues)
  }
}
