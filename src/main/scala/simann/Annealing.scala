package simann

import scalaz.Scalaz._
import scalaz.State

trait Annealable[A] {
  def heat(a: A): State[Random, A]
  def energy(a: A): Double
}

case class Temperature(value: Double) extends AnyVal {
  def *(x: Double) = Temperature(value * x)
}

case class AnnealingConfig(initialTemperature: Temperature, finalTemperature: Temperature, temperatureDropRatio: Double, stepsAtEachTemperature: Int)

object Annealing {
  def anneal[A: Annealable](start: A, config: AnnealingConfig): State[Random, Option[A]] = {
    val annealable = implicitly[Annealable[A]]
    val temperatures = unfold(config.initialTemperature)(t => (t.value > config.finalTemperature.value) ? (t, (t * config.temperatureDropRatio)).some | None)
    def acceptanceProbability(temperature: Temperature) = (d: Double) => math.exp(-d / temperature.value)
    def chooseTrialOrCurrent(trialSolution: A, current: A, temperature: Temperature): State[Random, A] = {
      val threshold = Random.nextDouble
      threshold.map(th => choose(trialSolution, current, acceptanceProbability(temperature), th, annealable.energy _))
    }
    anneal(start, temperatures, chooseTrialOrCurrent _, config.stepsAtEachTemperature)
  }

  def anneal[A: Annealable](start: A, temperatures: Stream[Temperature], chooseTrialOrCurrent: (A, A, Temperature) => State[Random, A], stepsAtEachTemperature: Int): State[Random, Option[A]] = {
    val trialSolutions: List[State[Random, A]] = temperatures.map { temperature =>
      (0 until stepsAtEachTemperature).toList.foldLeftM[({type M[B] = State[Random, B]})#M, A](start){case (current, _) => trySolution(current, temperature, chooseTrialOrCurrent)}
    }.toList
    val sequenced: State[Random, List[A]] = trialSolutions.sequenceU
    val annealable = implicitly[Annealable[A]]
    sequenced.map(_.find(a => annealable.energy(a) === 0))
  }

  private def trySolution[A: Annealable](current: A, temperature: Temperature, chooseTrialOrCurrent: (A, A, Temperature) => State[Random, A]): State[Random, A] = {
    val annealable = implicitly[Annealable[A]]
    val trialSolution = annealable.heat(current)
    trialSolution.flatMap(ts => chooseTrialOrCurrent(ts, current, temperature))
  }

  def choose[A](trialSolution: A, current: A, acceptanceProbability: Double => Double, threshold: => Double, energy: A => Double): A = {
    val improvement = energy(trialSolution) - energy(current)
    if (improvement < 0) {
      trialSolution
    } else {
      val calc = acceptanceProbability(improvement)
      (calc > threshold) ? trialSolution | current
    }
  }
}
