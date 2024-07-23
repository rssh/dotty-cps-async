package injection.examples.randomgen

import cats.effect.IO

package object generator {
  type IntUniqueSequenceGenerator = UniqueSequenceGenerator[IO, Int]
}
