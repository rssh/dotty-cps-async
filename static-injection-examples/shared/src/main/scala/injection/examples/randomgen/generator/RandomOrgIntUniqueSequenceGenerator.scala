package injection.examples.randomgen.generator

import cats.effect.IO
import cps.monads.catsEffect.given
import cps.{*, given}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

object RandomOrgIntUniqueSequenceGenerator extends IntUniqueSequenceGenerator {
  override def generate(startExclusive: Int, endInclusive: Int)(n: Int): IO[List[Int]] = async[IO] {
    val url = s"https://www.random.org/sequences/?min=$startExclusive&max=${endInclusive-1}&col=1&format=plain&rnd=new"
    val elements = JsoupBrowser().get(url).body.text
    elements.split("\\s+").toList.map(_.toInt).take(n)
  }
}
