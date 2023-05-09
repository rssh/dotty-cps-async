package cps.plugin.annotation

import scala.annotation.StaticAnnotation

/**
 *  mark that the function, whcih accept CpsMonadContext as parameter,
 *  but signature does not changed during cps transformation.
 *
 *  (such function live outside of cps-transformed code and can't contain suspend/await/reflect/...)
 *  It is usefull for intergnal implementation of context frameworks, FFI and integration with non-cps code.
 **/
case class CpsNotChange() extends StaticAnnotation
