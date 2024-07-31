package injection.examples.randomgen.plain.generator

import cats.effect.Sync
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

class RandomOrgIntUniqueSequenceGenerator[F[_] : Sync] extends IntUniqueSequenceGenerator[F] {
  override def generate(startExclusive: Int, endInclusive: Int)(n: Int): F[List[Int]] = Sync[F].delay {
    val url = s"https://www.random.org/sequences/?min=$startExclusive&max=${endInclusive - 1}&col=1&format=plain&rnd=new"
    val elements = JsoupBrowser().get(url).body.text
    elements.split("\\s+").toList.map(_.toInt).take(n)
  }
}

given [F[_] : Sync]: IntUniqueSequenceGenerator[F] = RandomOrgIntUniqueSequenceGenerator[F]
