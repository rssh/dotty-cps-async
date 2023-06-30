package cps.plugin.scaffolding

import scala.annotation.compileTimeOnly


/**
 * brakcet for a.
 * Used when we have one cps transformation inside another and whant to protect
 *
 * @param a
 * @tparam A
 * @return
 */
@compileTimeOnly("bracket is compile time only")
def bracket[A](a:A):A = a

