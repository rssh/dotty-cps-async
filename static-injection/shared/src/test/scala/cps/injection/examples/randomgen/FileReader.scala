package cps.injection.examples.randomgen

import java.io.File

abstract class FileReader[F[_]](file: File) {
  def readAll(file: File): F[String]
  def readAllLines(file: File): F[List[String]]
}


