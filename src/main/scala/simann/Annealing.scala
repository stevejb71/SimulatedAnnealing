package simann

import scalaz.Scalaz._
import util.Random

trait Annealable[A] {
  def tweak(a: A, r: Random): A
  def energy(a: A): Double
}

case class AnnealingConfig(initialTemperature: Double, finalTemperature:Double, alpha: Double, stepsPerChange: Int)

object Annealing {
  def anneal[A: Annealable](start: A, random: Random, config: AnnealingConfig): Option[A] = {
    val annealable = implicitly[Annealable[A]]
    val temperatures = unfold(config.initialTemperature)(t => (t > config.finalTemperature) ?? (t, (t * config.alpha)).some)
    def acceptanceProbability(temperature: Double) = (d: Double) => math.exp(-d / temperature)
    val trialSolutions = temperatures.map { temperature => {
      (0 until config.stepsPerChange).foldLeft(start){case (a, _) => oneStep(a, acceptanceProbability(temperature), random, annealable)}
    }}
    trialSolutions.find(a => annealable.energy(a) === 0)
  }

  def oneStep[A](current: A, acceptanceProbability: Double => Double, random: Random, annealable: Annealable[A]) = {
    val tweaked = annealable.tweak(current, random)
    val delta = annealable.energy(tweaked) - annealable.energy(current)
    if (delta < 0) {
      tweaked
    } else {
      val test = random.nextDouble()
      val calc = acceptanceProbability(delta)
      (calc > test) ? tweaked | current
    }
  }
}
