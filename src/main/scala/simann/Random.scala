package simann

import scalaz.State

case class Random(seed: Long)

object Random {
  def nextInt: State[Random, Int] = State { r =>
    val mask = (1L << 48) - 1
    val nextSeed = (r.seed * 0x5DEECE66DL + 0xBL) & mask
    (Random(nextSeed), (nextSeed >>> 16).asInstanceOf[Int])
  }
}
