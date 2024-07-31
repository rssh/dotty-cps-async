package injection.examples.randomgen.simple.generator

import cats.effect.Sync

class ScalaRandomIntUniqueSequenceGenerator[F[_] : Sync] extends IntUniqueSequenceGenerator[F] {

  override def generate(startExclusive: Int, endInclusive: Int)(n: Int): F[List[Int]] = Sync[F].delay {
    val rangeList = (startExclusive until endInclusive).toList
    util.Random.shuffle(rangeList).take(n)
  }
}
