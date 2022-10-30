package cps.plugin

import dotty.tools.dotc.util.SrcPos


case class CpsTransformException(message:String, pos: SrcPos) extends RuntimeException(message)