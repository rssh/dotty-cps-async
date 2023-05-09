package cps.plugin.annotation

import scala.annotation.StaticAnnotation

/**
 *  set debug level of CpsPlugin inside of class of method.
 *  (0 - No debug,  > 15 - trace all)
 **/
case class CpsDebugLevel(level:Int) extends StaticAnnotation