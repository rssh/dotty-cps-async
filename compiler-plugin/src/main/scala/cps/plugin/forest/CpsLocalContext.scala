package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Constants.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import core.Names.*
import ast.tpd.*
import cps.plugin.*

case class CpsLocalContext(nesting: Int, muted: Boolean)

