package injection.examples.randomgen.generator

import cats.effect.IO

object ScalaRandomIntUniqueSequenceGenerator extends IntUniqueSequenceGenerator {

  override def generate(startExclusive: Int, endInclusive: Int)(n: Int): IO[List[Int]] = IO {
    val rangeList = (startExclusive until endInclusive).toList
    util.Random.shuffle(rangeList).take(n)
  }
}
