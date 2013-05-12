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
    val temperatures = unfold(config.initialTemperature)(t => (t.value > config.finalTemperature.value) ? (t, (t * config.temperatureDropRatio)).some | None)
    anneal(start, temperatures, config.stepsAtEachTemperature)
  }

  def anneal[A: Annealable](start: A, temperatures: Stream[Temperature], stepsAtEachTemperature: Int): State[Random, Option[A]] = {
    val trialSolutions: Stream[State[Random, A]] = temperatures.map { temperature =>
      (0 until stepsAtEachTemperature).toList.foldLeftM[({type M[B] = State[Random, B]})#M, A](start){case (current, _) => testTrialSolution(current, temperature)}
    }
    val sequenced = trialSolutions.sequenceU
    val annealable = implicitly[Annealable[A]]
    sequenced.map(_.find(a => annealable.energy(a) === 0))
  }

  private def acceptanceProbability(temperature: Temperature) = (d: Double) => math.exp(-d / temperature.value)

  private def testTrialSolution[A: Annealable](current: A, temperature: Temperature): State[Random, A] = {
    val annealable = implicitly[Annealable[A]]
    val trialSolution = annealable.heat(current)
    for {
      ts <- trialSolution
      threshold <- Random.nextDouble
    } yield {
      choose(ts, current, acceptanceProbability(temperature), threshold, annealable.energy _)
    }
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
