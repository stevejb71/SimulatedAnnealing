package simann

import org.specs2.mutable._
import util.Random

class AnnealingSpec extends Specification {
  "next returns tweaked if it's better than current" in {
    val acceptanceProbability = (x: Double) => x
    val random = new Random(100)
    val annealable = new Annealable[Int] {
      def tweak(a: Int, r: Random): Int = 50
      def energy(a: Int): Double = a * 10.0
    }

    Annealing.oneStep(100, acceptanceProbability, random, annealable) must be_=== (50)
  }

}
