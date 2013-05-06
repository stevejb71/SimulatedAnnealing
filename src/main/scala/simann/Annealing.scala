package simann

import scalaz.Scalaz._
import util.Random

trait Annealable[A] {
  def heat(a: A, nextInt: Int => Int): A
  def energy(a: A): Double
}

case class AnnealingConfig(initialTemperature: Double, finalTemperature:Double, alpha: Double, stepsPerChange: Int)

object Annealing {
  def anneal[A: Annealable](start: A, random: Random, config: AnnealingConfig): Option[A] = {
    val annealable = implicitly[Annealable[A]]
    val temperatures = unfold(config.initialTemperature)(t => (t > config.finalTemperature) ?? (t, (t * config.alpha)).some)
    def acceptanceProbability(temperature: Double) = (d: Double) => math.exp(-d / temperature)
    val trialSolutions = temperatures.map { temperature => {
      (0 until config.stepsPerChange).foldLeft(start){case (current, _) => {
        val trialSolution = annealable.heat(current, random.nextInt _)
        choose(trialSolution, current, acceptanceProbability(temperature), random.nextDouble _, annealable.energy _)
      }}
    }}
    trialSolutions.find(a => annealable.energy(a) === 0)
  }

  def choose[A](trialSolution: A, current: A, acceptanceProbability: Double => Double, nextDouble: () => Double, energy: A => Double) = {
    val improvement = energy(trialSolution) - energy(current)
    if (improvement < 0) {
      trialSolution
    } else {
      val threshold = nextDouble()
      val temperature = acceptanceProbability(improvement)
      (temperature > threshold) ? trialSolution | current
    }
  }
}
