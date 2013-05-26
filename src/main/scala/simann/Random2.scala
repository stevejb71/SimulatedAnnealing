package simann

import scalaz._, Scalaz._

object Namespace {
  trait RandomGen {
    def next(bits: Int): (Int, RandomGen)
  }

  trait Random[A] {
    def randomR(lo: A, hi: A)(g: RandomGen): (A, RandomGen)
    def random(g: RandomGen): (A, RandomGen)
  }

  case class StdGen(seed: Long) extends RandomGen {
    def next(bits: Int): (Int, RandomGen) = {
      val mask = (1L << 48) - 1
      val nextSeed = (seed * StdGen.multiplier + StdGen.addend) & mask
      ((nextSeed >>> (48 - bits)).asInstanceOf[Int], StdGen(nextSeed))
    }
  }

  object StdGen {
    val multiplier = 0x5DEECE66DL
    val addend = 0xBL
    val mask = (1L << 48) - 1
    def scramble(initialSeed: Long) = StdGen((initialSeed ^ multiplier) & mask)
  }

  implicit val randomBool = new Random[Boolean] {
    override def randomR(lo: Boolean, hi: Boolean)(g: RandomGen): (Boolean, RandomGen) = (lo === hi) ? (lo, g) | random(g)
    override def random(g: RandomGen): (Boolean, RandomGen) = ((_:Int) === 0).first(g.next(1))
  }

  implicit val randomInt = new Random[Int] {
    override def randomR(lo: Int, hi: Int)(g: RandomGen): (Int, RandomGen) = {
      val n = hi - lo
      if ((n & -n) == n) {
        val (r, g2) = g.next(31)
        (((n * r.asInstanceOf[Long]) >> 31).asInstanceOf[Int], g2)
      } else {
        var value = 0
        var g2 = g
        var bits = 0
        do {
          val nextRnd = g2.next(31)   // (1566086245,StdGen(205270056366173))
          bits = nextRnd._1
          g2 = nextRnd._2
          value = bits % n
        } while (bits - value + (n - 1) < 0)
        (value, g2)
      }
    }
    override def random(g: RandomGen): (Int, RandomGen) = g.next(32)
  }

  implicit val randomDouble = new Random[Double] {
    def randomR(lo: Double, hi: Double)(g: RandomGen): (Double, RandomGen) = ???

    def random(g: RandomGen): (Double, RandomGen) = ???
  }
}
