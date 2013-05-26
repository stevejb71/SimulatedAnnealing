package simann

import scalaz._, Scalaz._

object Namespace {
  trait RandomGen {
    def next(bits: Int): (RandomGen, Int)
  }

  trait Random[A] {
    def randomR(lo: A, hi: A)(g: RandomGen): (RandomGen, A)
    def random(g: RandomGen): (RandomGen, A)
  }

  case class StdGen(seed: Long) extends RandomGen {
    def next(bits: Int): (RandomGen, Int) = {
      val mask = (1L << 48) - 1
      val nextSeed = (seed * StdGen.multiplier + StdGen.addend) & mask
      (StdGen(nextSeed), (nextSeed >>> (48 - bits)).asInstanceOf[Int])
    }
  }

  object StdGen {
    val multiplier = 0x5DEECE66DL
    val addend = 0xBL
    val mask = (1L << 48) - 1
    def scramble(initialSeed: Long) = StdGen((initialSeed ^ multiplier) & mask)
  }

  implicit val randomBool = new Random[Boolean] {
    override def randomR(lo: Boolean, hi: Boolean)(g: RandomGen) = (lo === hi) ? (g, lo) | random(g)
    override def random(g: RandomGen) = ((_:Int) /== 0).second(g.next(1))
  }

  implicit val randomInt = new Random[Int] {
    override def randomR(lo: Int, hi: Int)(g: RandomGen) = {
      val n = hi - lo
      if ((n & -n) == n) {
        val (g2, r) = g.next(31)
        (g2, ((n * r.asInstanceOf[Long]) >> 31).asInstanceOf[Int])
      } else {
        var value = 0
        var g2 = g
        var bits = 0
        do {
          val nextRnd = g2.next(31)
          g2 = nextRnd._1
          bits = nextRnd._2
          value = bits % n
        } while (bits - value + (n - 1) < 0)
        (g2, value)
      }
    }
    override def random(g: RandomGen) = g.next(32)
  }

  implicit val randomDouble = new Random[Double] {
    def randomR(lo: Double, hi: Double)(g: RandomGen) = (lo + (hi - lo) * (_: Double)).second(uniform(g))
    def random(g: RandomGen) = randomR(Double.MinValue, Double.MaxValue)(g)
    private[this] def uniform(g: RandomGen): (RandomGen, Double) = {
      val next26 = ((_:Int).asInstanceOf[Long] << 27).second(g.next(26))
      val next27 = next26._1.next(27)
      (next27._1, (next26._2 + next27._2) / (1L << 53).asInstanceOf[Double])
    }
  }

  def MonadRandomT[M[+_], A](lo: A, hi: A)(g: RandomGen)(implicit R: Random[A], M: Monad[M]) = StateT((g: RandomGen) => M.pure(R.randomR(lo, hi)(g)))
  def MonadRandom[A: Random](lo: A, hi: A)(g: RandomGen) = MonadRandomT[Identity, A](lo, hi)(g)
}
