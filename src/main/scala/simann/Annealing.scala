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
    val trialSolutions = temperatures.map { temperature => {
      (0 until config.stepsPerChange).foldLeft(start){case (a, _) => next(a, temperature, random)}
    }}
    trialSolutions.find(a => annealable.energy(a) === 0)
  }

  def next[A: Annealable](current: A, temperature: Double, random: Random) = {
    val annealable = implicitly[Annealable[A]]
    val tweaked = annealable.tweak(current, random)
    val delta = annealable.energy(tweaked) - annealable.energy(current)
    if (delta < 0) {
      tweaked
    } else {
      val test = random.nextDouble()
      val calc = math.exp(-delta / temperature)
      (calc > test) ? tweaked | current
    }
  }
}
