package cps.plugin.observatory

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.SrcPos


class ValUsage(
                var optValDef: Option[ValDef] = None,
                val inAwaits: ArrayBuffer[Tree] = ArrayBuffer(),
                val withoutAwaits: ArrayBuffer[Tree] = ArrayBuffer(),
                val aliases: ArrayBuffer[ValUsage] = ArrayBuffer()
              ) {

  def nInAwaits: Int = inAwaits.length + aliases.map(_.nInAwaits).sum

  def nWithoutAwaits: Int = withoutAwaits.length + aliases.map(_.nWithoutAwaits).sum

  def allInAwaits: Seq[Tree] =
    (Seq.empty ++ inAwaits.toSeq ++ aliases.toSeq.flatMap(_.allInAwaits))

  def allWithoutAwaits: Seq[Tree] =
    (Seq.empty ++ withoutAwaits.toSeq ++ aliases.toSeq.flatMap(_.allWithoutAwaits))

  def definedInside: Boolean = !optValDef.isEmpty

  def reportCases()(using Context): Unit =
    val firstWithout = allWithoutAwaits.headOption
    val firstWith = allInAwaits.headOption
    for (t <- firstWithout) {
      report.inform("async usage", t.srcPos)
    }
    for (t <- firstWith) {
      report.inform("usage with await", t.srcPos)
    }

}


// TODO: deprecated after CpsDirect context will be grown up from an experiment
class AutomaticColoringAnalyzer {

  val usageRecords = new MutableSymbolMap[ValUsage]()


  def observe(tree: Tree)(using context: Context): Unit = {

    val traverser = new TreeTraverser {

      override def traverse(tree: Tree)(using ctx: Context): Unit = {
        tree match {
          case tree: ValDef =>
            if (tree.symbol.is(Flags.Mutable)) {
              report.error("Mutable var is not supported in the block with automatic coloring", tree.srcPos)
            }
            val usage = usageRecords.getOrElseUpdate(tree.symbol, ValUsage())
            usage.optValDef = Some(tree)
            println(s"AutomaticColoringAnalyzer: found valDef: ${tree.symbol.showFullName},  id=${tree.symbol.hashCode()} rhs=${tree.rhs}")
            tree.rhs match
              case id: Ident =>
                val usageRecord = usageRecords.getOrElseUpdate(id.symbol, ValUsage())
                usageRecord.aliases += usage
                println(s"AutomaticColoringAnalyzer: found alias: ${id.symbol.showFullName} for ${tree.symbol} (${tree.symbol.hashCode()})")
              case _ =>
                println(s"AutomaticColoringAnalyzer: traversing rhs")
                traverse(tree.rhs)(using ctx.withOwner(tree.symbol))
          case term@Apply(fun, args) =>
            // to have the same structure as forest/ApplyTransform for the same patterns
            checkApply(term, fun, args)
          case id: Ident =>
            val usageRecord = usageRecords.getOrElseUpdate(id.symbol, ValUsage())
            usageRecord.withoutAwaits += id
          case _ =>
            println("AutomaticColoringAnalyzer: traversing children begin")
            super.traverseChildren(tree)
            println("AutomaticColoringAnalyzer: traversing children end")
        }
      }

      def checkApply(tree: Apply, fun: Tree, args: List[Tree])(using ctx: Context): Unit = {
        tree match
          case ImplicitAwaitCall(arg, tg, ta, tf, gc, gcn) =>
            println(s"AutomaticColoring:discovered await: arg=${arg.show}")
            arg match
              case id: Ident =>
                println("arg is ident")
                val usageRecord = usageRecords.getOrElseUpdate(id.symbol, ValUsage())
                usageRecord.inAwaits += id
              case _ =>
                println("arg is not ident")
                super.traverseChildren(tree)
          case Apply(Apply(TypeApply(cnAwait, targs), List(arg)), List(monad, conversion))
            if cnAwait.symbol == Symbols.requiredMethod("cps.await") =>
            println("discovered await: arg=$arg")
            arg match
              case id: Ident =>
                val usageRecord = usageRecords.getOrElseUpdate(id.symbol, ValUsage())
                usageRecord.inAwaits += id
              case _ =>
                super.traverseChildren(tree)
          case _ =>
            super.traverseChildren(tree)
      }


    }
    traverser.traverse(tree)


  }




}