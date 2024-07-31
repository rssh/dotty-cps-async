package injection.examples.randomgen.plain

package object generator {
  type IntUniqueSequenceGenerator[F[_]] = UniqueSequenceGenerator[F, Int]
}
