package cps.macros.forest

import scala.quoted._
import scala.util.control.NonFatal

import cps._
import cps.macros._
import cps.macros.misc._
import cps.macros.common._

/**
 * BlockTreeTransform -- the same as BlockTransform but on term level.
 * (yet not enabled)
 **/
trait BlockTreeTransform[F[_],CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._  

  def runBlock(block: Block, prevs: List[Statement], last: Term)(owner:Symbol): CpsTree = {
    if (prevs.isEmpty) then
      runRoot(last)(owner)
    else
      val prevsCpsTrees = prevs.map{
        case d: Definition =>
            d match
              case v@ValDef(vName,vtt,optRhs) => runValDefFromBlock(block, v)(owner)
              case _ => PureCpsTree(owner,d)
        case t: Term =>
            blockApplyValueDiscard(t)(owner)
        case i: Import => CpsTree.empty
        case other => 
          throw MacroError(s"unknown tree type in block: $other", block.asExpr)
      }
      val lastCps = runRoot(last)(owner)
      val prevsCps: CpsTree = prevsCpsTrees.foldLeft(CpsTree.empty){ (s,e) => s.append(e) }
      prevsCps.appendFinal(lastCps)
  }

  def blockApplyValueDiscard(t:Term)(owner: Symbol): CpsTree = {
    if (blockCheckValueDiscarded(t)) then
      if (cpsCtx.flags.customValueDiscard) then
        val valueDiscard = TypeIdent(Symbol.classSymbol("cps.ValueDiscard")).tpe
        val tpe = t.tpe.widen.dealias
        val tpTree = valueDiscard.appliedTo(tpe)
        Implicits.search(tpTree) match
          case sc: ImplicitSearchSuccess =>
            val pd = {
               if sc.tree.tpe <:< TypeRepr.of[cps.AwaitValueDiscard[?,?]] then
                 blockBuildAwaitValueDiscard(sc.tree, t)
               else
                Apply(Select.unique(sc.tree,"apply"),List(t))
            }
            runRoot(pd)(owner)
          case fl: ImplicitSearchFailure =>
            val tps = safeTypeShow(tpTree)
            val msg = s"discarding non-unit value without custom discard $tps (${fl.explanation})"
            if (cpsCtx.flags.warnValueDiscard) then
               report.warning(msg, t.pos)
            else
               report.error(msg, t.pos)
            runRoot(t)(owner)
      else
        report.warning(s"discarding non-unit value ${t.show}", t.pos)
        runRoot(t)(owner)
    else
      runRoot(t)(owner)
  }

  def blockCheckValueDiscarded(t: Term): Boolean =
    ValueDiscardHelper.checkValueDiscarded(t, cpsCtx.flags)


  def blockBuildAwaitValueDiscard(discardTerm: Term, p: Term): Term =
  
    def parseDiscardTermType(tpe: TypeRepr): (TypeRepr, TypeRepr) =
        tpe match
           case AppliedType(base, targs) =>
                  base match
                    case TypeRef(sup, "AwaitValueDiscard") =>
                       targs match
                         case List(tf, tt) => (tf, tt)
                         case _ =>
                             val msg = s"Expected that AwaitValueDiscard have 2 type paraleters, but we have $targs"
                             throw MacroError(msg, discardTerm.asExpr)
                    case _ =>
                       val msg = s"Reference to AwaitValueDiscard expected"
                       throw MacroError(msg, discardTerm.asExpr)
           case _ =>
                  val msg = s"Can't parse AwaitValueDiscard type, tpe=${tpe}"
                  throw MacroError(msg, discardTerm.asExpr)

    discardTerm.tpe.asType match
        case '[AwaitValueDiscard[F,tt]] =>
           val refP = p.asExprOf[F[tt]]
           '{  await[F,tt,F]($refP)(using ${cpsCtx.monadContext}, CpsMonadConversion.identityConversion[F])  }.asTerm
           //bug in dotty. TODO: submit
           //case '[AwaitValueDiscard[[xt]=>>ft,tt]] =>
           //   ???
        case _ => 
           val (ftr, ttr) = parseDiscardTermType(discardTerm.tpe)
           val ftmt = TypeRepr.of[CpsMonad].appliedTo(ftr)
           Implicits.search(ftmt) match
              case monadSuccess: ImplicitSearchSuccess =>
                val ftm = monadSuccess.tree
                Apply(    
                     Apply(
                       TypeApply(Ref(Symbol.requiredMethod("cps.await")), 
                          List(Inferred(ftr),Inferred(ttr),Inferred(ftr))),
                       List(p)
                     ),
                     List(ftm, cpsCtx.monadContext.asTerm)
                )
              case monadFailure: ImplicitSearchFailure =>
                throw MacroError(s"Can't find appropriative monad for ${discardTerm.show}, ${monadFailure.explanation}  : ", p.asExpr)




