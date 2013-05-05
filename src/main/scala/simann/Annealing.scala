package simann

import scala.util.Random
import scalaz.Scalaz._

trait Annealable[A] {
  def tweak(a: A, r: Random): A
  def energy(a: A): Double
}

case class AnnealingConfig(initialTemperature: Double, finalTemperature:Double, alpha: Double, stepsPerChange: Int)

object Annealing {
  def anneal[A: Annealable](board: A, random: Random, config: AnnealingConfig): Option[A] = {
    val annealable = implicitly[Annealable[A]]
    val temperatures = unfold(config.initialTemperature)(t => (t > config.finalTemperature) ?? (t, (t * config.alpha)).some)
    val trialBoards = temperatures.map { temperature => {
      def nextBoard(currentBoard: A) = {
        val workingBoard = annealable.tweak(currentBoard, random)
        val delta = annealable.energy(workingBoard) - annealable.energy(currentBoard)
        if (delta < 0) {
          workingBoard
        } else {
          val test = random.nextDouble()
          val calc = math.exp(-delta / temperature)
          (calc > test) ? workingBoard | currentBoard
        }
      }

      (0 until config.stepsPerChange).foldLeft(board){case (b, _) => nextBoard(b)}
    }}
    trialBoards.find(b => annealable.energy(b) === 0)
  }


}
