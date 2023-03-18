package cps.plugin

import scala.annotation.*
import dotty.tools.dotc.*
import core.*
import core.Decorators.*
import core.Contexts.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import plugins.*
import cps.plugin.QuoteLikeAPI.*
import cps.plugin.forest.*
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.util.SrcPos

class PhaseCps(shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  val phaseName = "rssh.cps"

  override def allowsImplicitSearch = true
  override val runsAfter = Set("cc")
  override val runsBefore = Set("rssh.cpsAsyncShift")

  val debug = true


  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = {
    try
      transformDefDefInternal(tree)
    catch
      case ex: CpsTransformException =>
        report.error(ex.message, ex.pos)
        //ex.printStackTrace()
        throw ex;
  }

  def transformDefDefInternal(tree: DefDef)(using Context): Tree = {
    // we should transform any def-def which accept CpsMonadContext as context parameter.
    // This is both DefDef-s in lambda functions and fucntions with return valie
    // CpsMonadContext[F] ?=> T.
    //
    // Note, that lambda-function can be catched before this in transformBlock
    // if current transformation will be incorrect, because we need to change the
    // type of function.
    //
    //  But we can change function type only inside cps transformation, which will be handled
    //   via recursive parsing.
    //
    //  TODO: Think about cc=analysis, direct call of functions with using parameters
    findCpsMonadContextParam(tree.paramss, tree.srcPos) match
      case Some(cpsMonadContext) =>
        println(s"transforming ${tree.symbol.showFullName}")
        //println(s"${tree.show}")
        val (monadValDef, tc) = makeMonadValAndCpsTopLevelContext(cpsMonadContext,tree)
        val nTpt = CpsTransformHelper.cpsTransformedType(tree.tpt.tpe, tc.monadType)
        given CpsTopLevelContext = tc
        val transformedRhs = RootTransform(tree.rhs,tree.symbol,0).transformed
        val nRhs = Block(monadValDef::Nil,transformedRhs)
        val retval = cpy.DefDef(tree)(tree.name, tree.paramss, TypeTree(nTpt), nRhs)
        //println(s"transformDefDef result: ${retval.show}")
        retval
      case None =>
        // check return type to catch function wich return params
        if (Symbols.defn.isContextFunctionType(tree.tpt.tpe)) then
          tree.rhs match
            case CheckLambda(params,body,bodyOwner) =>
              findCpsMonadContextParam(List(params), tree.rhs.srcPos) match
                case Some(cpsMonadContext) =>
                  if (true) {
                    println(s"transforming ${tree.symbol.showFullName}")
                    //println(s"${tree.show}")
                    //println(s"transformDefDef:lambda found as resulting expression")
                  }
                  val (monadValDef, tc) = makeMonadValAndCpsTopLevelContext(cpsMonadContext, tree)
                  val mt = CpsTransformHelper.transformContextualLambdaType(tree.rhs,params,body,tc.monadType)
                  val meth = Symbols.newAnonFun(tree.symbol,mt)
                  val nRhs = Closure(meth, tss => {
                    given CpsTopLevelContext = tc
                    val cpsTree = RootTransform(body, bodyOwner, 0)
                    val transformedBody = Block(List(monadValDef.changeOwner(summon[Context].owner,bodyOwner)), cpsTree.transformed)
                    TransformUtil.substParams(transformedBody, params, tss.head).changeOwner(bodyOwner, meth).withSpan(body.span)
                  })
                  val nTpt = tree.tpt.tpe.widen match
                    case AppliedType(tycon,targs) =>
                       AppliedType(tycon,CpsTransformHelper.adoptResultTypeParam(targs,tc.monadType))
                    case _ =>
                       throw CpsTransformException("Expected context function type as applied type",tree.srcPos)
                  val retval = cpy.DefDef(tree)(rhs=nRhs,tpt=TypeTree(nTpt))
                  //println(s"transformDefDef result: ${retval.show}")
                  retval
                case _ =>
                  super.transformDefDef(tree)
            case _ =>
              super.transformDefDef(tree)
        else
          super.transformDefDef(tree)
  }


  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    try
      transformApplyInternal(tree)
    catch
      case ex: CpsTransformException =>
        report.error(ex.message,ex.pos)
        throw ex;
  }

  def transformApplyInternal(tree: Apply)(using Context):Tree = {
    tree match
      case Apply(
            TypeApply(Select(infernAsyncArgCn,applyCn),List(aCnd)),
            List(CheckLambda(params,body,bodyOwner))
         )  if applyCn.mangledString == "apply"
           =>
            infernAsyncArgCn.tpe match
              case AppliedType(tycon, tpargs) 
                if tycon.typeSymbol == Symbols.requiredClass("cps.CpsTransform.InfernAsyncArg") =>
                  println(s"found CpsAsync candidate, tycon.typeSymbol=:  ${tycon.typeSymbol} ")
                  println(s"CpsAsync.body=:  ${body.show} ")
                  println(s"CpsAsync.bodyType=:  ${body.tpe.show} ")
                  try
                    val cpsTransformType = params(0).tpt.tpe
                    val monadType = CpsTransformHelper.extractMonadType(cpsTransformType,tree.srcPos)
                    val optRuntimeAwait = CpsTransformHelper.findRuntimeAwait(monadType, tree.span)
                    val mt = MethodType(List(params(0).name))(
                      x => List(params(0).tpt.tpe),
                      //x => tree.tpe.widen  //decorateTypeApplications(monadType).appliedTo(body.tpe)
                      x => decorateTypeApplications(monadType).appliedTo(body.tpe)
                    )
                    // TODO:  pass am to apply 
                    val amInit = Select(infernAsyncArgCn,"am".toTermName)
                    // TODO: changeOwnwe ?
                    val amValDef = SyntheticValDef("m".toTermName,amInit)
                    val am = ref(amValDef.symbol)
                    val meth = Symbols.newAnonFun(summon[Context].owner,mt)
                    val ctxFun = Closure(meth, tss => {
                                          // here = check that valDef constructire chaned amOwner
                      val tc = CpsTopLevelContext(monadType,am,params(0),optRuntimeAwait,DebugSettings.make(tree))
                      given CpsTopLevelContext = tc
                      println(s"cpsAsync:tc, debugLevel=${tc.settings.debugLevel}")
                      val cpsTree = RootTransform(body,bodyOwner, 0)
                      val transformedBody = Block(amValDef::Nil, cpsTree.transformed)
                      TransformUtil.substParams(transformedBody,List(params(0)),tss.head)
                                    .changeOwner(bodyOwner,meth)
                                    .withSpan(body.span)
                    }) 
                    val apply = Apply(
                                TypeApply(Select(am,"apply".toTermName),List(TypeTree(body.tpe.widen))),
                                List(ctxFun)
                    ).withSpan(tree.span)
                    val retval = Block(
                      List(amValDef),
                      apply
                    )
                    println(s"origin cpsAsync tree: ${tree.show}")
                    println(s"transformed cpsAwait tree: ${retval.show}")
                    println(s"ctxFun=${ctxFun.show}")

                    retval
                  catch
                    case ex:CpsTransformException =>
                      report.error(ex.message,ex.pos)
                      if (debug) {
                        ex.printStackTrace()
                      }
                      tree
              case _ =>
                  super.transformApply(tree)
      case _ => super.transformApply(tree)

 
  }



  private def findCpsMonadContextParam(value: List[Trees.ParamClause[Types.Type]], srcPos: SrcPos)(using Context): Option[Tree] = {
    findAllCpsMonadContextParam(value,List.empty) match
      case head::Nil => Some(head)
      case head::tail =>
        // later we can combine many contexts ar one using effect stacks or monad transformeds.
        throw CpsTransformException("Few monadcontexts in one function is not supported yet",srcPos)
      case Nil => None
  }

  @tailrec
  private def findAllCpsMonadContextParam(paramss: List[Trees.ParamClause[Types.Type]],
                                          acc: List[ValDef])(using Context): List[Tree] = {
    paramss match
      case paramssHead::paramssTail =>
        paramssHead match
          case paramsHead::paramTail =>
            paramsHead match
              case vd: ValDef =>
                val filtered = paramssHead.asInstanceOf[List[ValDef]].filter((p: ValDef) => CpsTransformHelper.isCpsMonadContextType(p.tpt.tpe))
                findAllCpsMonadContextParam(paramssTail, filtered ++ acc)
              case _ =>
                findAllCpsMonadContextParam(paramssTail, acc)
          case Nil =>
            findAllCpsMonadContextParam(paramssTail, acc)
      case Nil =>
        acc
  }

  private def makeMonadValAndCpsTopLevelContext(cpsMonadContext: Tree, tree: Tree)(using Context): (Tree, CpsTopLevelContext) =  {
    val monadInit = Select(cpsMonadContext, "monad".toTermName).withSpan(tree.span)
    val monadType = CpsTransformHelper.extractMonadType(cpsMonadContext.tpe.widen, tree.srcPos)
    val optRuntimeAwait = CpsTransformHelper.findRuntimeAwait(monadType, tree.span)
    //val insideContext =
    val monadValDef = SyntheticValDef("m".toTermName, monadInit).changeOwner(summon[Context].owner, tree.symbol)
    val monadRef = ref(monadValDef.symbol)
    val tc = CpsTopLevelContext(monadType, monadRef, cpsMonadContext, optRuntimeAwait, DebugSettings.make(tree))
    (monadValDef, tc)
  }


}

