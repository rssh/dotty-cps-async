package cps.injection.examples.randomgen

import cats.effect.IO

import java.io.File

abstract class Service[+F[_], T, E] {
  def findWinners(file: File, n: Int): F[Either[E, Seq[T]]]
}
