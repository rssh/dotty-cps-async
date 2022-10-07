package cps.plugin

import dotty.tools.dotc.util.SrcPos


class CpsTransformException(message:String, pos: SrcPos) extends RuntimeException