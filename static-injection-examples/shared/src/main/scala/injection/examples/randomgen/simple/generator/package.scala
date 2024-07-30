package injection.examples.randomgen.simple

package object generator {
  type IntUniqueSequenceGenerator[F[_]] = UniqueSequenceGenerator[F, Int]
}
