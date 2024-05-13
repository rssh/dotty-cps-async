package cps.plugin.annotation


import scala.annotation.{StaticAnnotation}

/**
 * Inserted by compiler plugin befor picker, to mark that this method was transformed by cps plugin.
 * Macro and cps plugin check existence of this annotation to prevent situation, when duriong compiling
 * one of modules was mistakenly compiled without cps plugin.
 */
case class CpsTransformed[F[_]]() extends StaticAnnotation
