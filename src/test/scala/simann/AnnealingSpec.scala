package simann

import org.specs2.mutable._

class AnnealingSpec extends Specification {
  private val nextDouble = () => 1.0
  private val energy = (a: Int) =>  a * 10.0

  "choose returns tweaked if it's better than current" in {
    val acceptanceProbability = (x: Double) => x
    Annealing.choose(50, 100, acceptanceProbability, nextDouble, energy) must be_=== (50)
  }

  "choose returns tweaked if it's worse but the temperature is greater than the threshold" in {
    val acceptanceProbability = Map(500.0 -> 1.1)
    Annealing.choose(150, 100, acceptanceProbability, nextDouble, energy) must be_=== (150)
  }

  "choose returns current if tweaked is worse and the temperature is not greater than the threshold" in {
    val acceptanceProbability = Map(500.0 -> 1.0)
    Annealing.choose(150, 100, acceptanceProbability, nextDouble, energy) must be_=== (100)
  }
}
