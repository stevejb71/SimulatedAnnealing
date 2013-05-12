package simann

import org.specs2.mutable._

class AnnealingSpec extends Specification {
  private val energy = (a: Int) =>  a * 10.0

  "choose returns trial solution if it's better than current" in {
    val acceptanceProbability = (x: Double) => x
    Annealing.choose(50, 100, acceptanceProbability, 1.0, energy) must be_=== (50)
  }

  "choose returns trial solution if it's worse but the temperature is greater than the threshold" in {
    val acceptanceProbability = Map(500.0 -> 1.1)
    Annealing.choose(150, 100, acceptanceProbability, 1.0, energy) must be_=== (150)
  }

  "choose returns trial solution if tweaked is worse and the temperature is not greater than the threshold" in {
    val acceptanceProbability = Map(500.0 -> 1.0)
    Annealing.choose(150, 100, acceptanceProbability, 1.0, energy) must be_=== (100)
  }
}
