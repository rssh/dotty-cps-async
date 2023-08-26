package cps.plugin.scaffolding

import scala.annotation.compileTimeOnly


@compileTimeOnly("dotty-cps-async-compiler-plugin should be enablet to use CpsDirect")
def requiringCpsCompilerPlugin[T](t:T):T = ???
