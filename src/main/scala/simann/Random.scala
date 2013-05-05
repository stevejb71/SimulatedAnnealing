package simann

import scalaz.State

case class FRandom(seed: Long)

object FRandom {
  def nextInt: State[FRandom, Int] = next(32)

  def nextDouble: State[FRandom, Double] = for {
    n26 <- next(26).map(i => (i.asInstanceOf[Long] << 27))
    n27 <- next(27)
  } yield ((n26 + n27) / (1L << 53).asInstanceOf[Double])

  private def next(bits: Int): State[FRandom, Int] = State { r =>
    val mask = (1L << 48) - 1
    val nextSeed = (r.seed * 0x5DEECE66DL + 0xBL) & mask
    (FRandom(nextSeed), (nextSeed >>> (48 - bits)).asInstanceOf[Int])
  }
}
