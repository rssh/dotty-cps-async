package injection.examples.randomgen

package object generator {
  type IntUniqueSequenceGenerator[F[_]] = UniqueSequenceGenerator[F, Int]
}
