package simann

import scalaz.Scalaz._
import scalaz.State

trait Annealable[A] {
  def heat(a: A, nextInt: Int => Int): A
  def energy(a: A): Double
}

case class Temperature(value: Double) extends AnyVal {
  def *(x: Double) = Temperature(value * x)
}

case class AnnealingConfig(initialTemperature: Temperature, finalTemperature: Temperature, temperatureDropRatio: Double, stepsAtEachTemperature: Int)

object Annealing {
  def anneal[A: Annealable](start: A, random: util.Random, config: AnnealingConfig): State[Random, Option[A]] = {
    val annealable = implicitly[Annealable[A]]
    val temperatures = unfold(config.initialTemperature)(t => (t.value > config.finalTemperature.value) ? (t, (t * config.temperatureDropRatio)).some | None)
    def acceptanceProbability(temperature: Temperature) = (d: Double) => math.exp(-d / temperature.value)
    val chooseTrialOrCurrent = (trialSolution: A, current: A, temperature: Temperature) => choose(trialSolution, current, acceptanceProbability(temperature), random.nextDouble _, annealable.energy _)
    anneal(start, random, temperatures, chooseTrialOrCurrent, config.stepsAtEachTemperature)
  }

  def anneal[A: Annealable](start: A, random: util.Random, temperatures: Stream[Temperature], chooseTrialOrCurrent: (A, A, Temperature) => A, stepsAtEachTemperature: Int): State[Random, Option[A]] = State {r =>
    val annealable = implicitly[Annealable[A]]
    val trialSolutions = temperatures.map { temperature => {
      (0 until stepsAtEachTemperature).foldLeft(start){case (current, _) => {
        val trialSolution = annealable.heat(current, random.nextInt _)
        chooseTrialOrCurrent(trialSolution, current, temperature)
      }}
    }}
    (r, trialSolutions.find(a => annealable.energy(a) === 0))
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
