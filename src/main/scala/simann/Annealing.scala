package simann

import scalaz.Scalaz._
import util.Random

trait Annealable[A] {
  def tweak(a: A, nextInt: Int => Int): A
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

  def oneStep[A](current: A, acceptanceProbability: Double => Double, random: Random, annealable: Annealable[A]): A = {
    val tweaked = annealable.tweak(current, random.nextInt _)
    choose(tweaked, current, acceptanceProbability, random.nextDouble _, annealable.energy _)
  }

  def choose[A](tweaked: A, current: A, acceptanceProbability: Double => Double, nextDouble: () => Double, energy: A => Double) = {
    val delta = energy(tweaked) - energy(current)
    if (delta < 0) {
      tweaked
    } else {
      val threshold = nextDouble()
      val temperature = acceptanceProbability(delta)
      (temperature > threshold) ? tweaked | current
    }
  }
}
