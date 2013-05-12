package simann

import scalaz._, Scalaz._, effect.IO

case class Random(seed: Long)

object Time {
  val currentTimeMillis = IO {
    System.currentTimeMillis()
  }
}

object RandomIO {
  val random: IO[Random] = Time.currentTimeMillis.map(Random(_))
}

object Random {
  val nextInt: State[Random, Int] = next(32)

  def nextInt(n: Int): State[Random, Int] = {
    if (n <= 0) {
      throw new IllegalArgumentException("n must be positive")
    }
    if ((n & -n) === n) {
      next(31).map((n: Int) => ((n * next(31).asInstanceOf[Long]) >> 31).asInstanceOf[Int])
    } else {
      def loop: State[Random, Int] = {
        val bits = next(31)
        val result = bits.map(_ % n)
        val tryAgain = for {
          b <- bits
          r <- bits.map(_ % n)
        } yield {
          b - r + (n - 1) < 0
        }
        tryAgain.ifM(loop, result)
      }
      loop
    }
  }

  val nextDouble: State[Random, Double] = for {
    n26 <- next(26).map(i => (i.asInstanceOf[Long] << 27))
    n27 <- next(27)
  } yield ((n26 + n27) / (1L << 53).asInstanceOf[Double])

  private def next(bits: Int): State[Random, Int] = State { r =>
    val mask = (1L << 48) - 1
    val nextSeed = (r.seed * 0x5DEECE66DL + 0xBL) & mask
    (Random(nextSeed), (nextSeed >>> (48 - bits)).asInstanceOf[Int])
  }
}
