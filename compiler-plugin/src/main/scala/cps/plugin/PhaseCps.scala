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

class PhaseCps(shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  val phaseName = "rssh.cps"

  override def allowsImplicitSearch = true
  override val runsAfter = Set("cc")
  override val runsBefore = Set("rssh.cpsAsyncShift")

  val debug = true


  override def transformDefDef(tree:DefDef)(using Context): Tree = {
    // TODO:
    //  find parameter with outer value CpsMonadContext[F] ?=> T
    //  Translate to function which return M[T] instead T which body is cps-transformed
    if (Symbols.defn.isContextFunctionType(tree.tpt.tpe)) then
       println(s"defDef: name=${tree.name}, tpt=${tree.tpt}")
       tree.tpt match
          case tt: TypeTree =>
            tt.typeOpt match
              case AppliedType(tycon, targs) =>
                println(s"AppliedType, args=$targs")
                revListCpsTransformInContextFunctionArgTypes(targs) match
                  case Nil =>
                    super.transformDefDef(tree)
                  case (cpsMonadContextType, argIndex)::Nil =>
                    tree.rhs match
                      case CheckLambda(params, body, bodyOwner) =>
                        try 
                          if (true) {
                            val sparams = params.map(_.show).mkString(",")
                            println(s"transformDefDef:lambda found as resulting expression, params=${sparams}},  rhs.tpe=${tree.rhs.tpe}")
                          }
                          val monadType = CpsTransformHelper.extractMonadType(cpsMonadContextType,tree.srcPos)
                          val cpsContextParam = params(argIndex)
                          //val monadInit = CpsTransformHelper.findImplicitInstance(monadType,tree.span).getOrElse(
                          //  throw CpsTransformException(s"Can't find an implicit instance of monad ${monadType.show}", tree.srcPos)
                          //)
                          val monadInit = Select(cpsContextParam,"monad".toTermName).withSpan(tree.span)
                          //val monadFieldName = "m".toTermName
                          //val monad = Select(cpsTransformParam, monadFieldName).withSpan(tree.span)
                          val optRuntimeAwait = CpsTransformHelper.findRuntimeAwait(monadType,tree.span)
                          //val oldMt = tree.rhs.tpe match
                          //  case v:MethodType => v
                          //  case _  => throw CpsTransformException(s"type of context function is not MethodType but ${tree.rhs.tpe}",tree.srcPos)
                          val mt = CpsTransformHelper.transformContextualLambdaType(tree.rhs,params,body,monadType)
                          val meth = Symbols.newAnonFun(summon[Context].owner,mt)
                          println(s"transformDefDef:creating new closure, type=${mt.show}")
                          val nRhs = Closure(meth,tss => {
                              val monadValDef = SyntheticValDef("m".toTermName,monadInit)
                              val monad = ref(monadValDef.symbol)
                              val tc = TransformationContext(monadType,monad,cpsContextParam,optRuntimeAwait)
                              val cpsTree = RootTransform(body, bodyOwner, tc)
                              val transformedBody = Block(List(monadValDef),cpsTree.transformed)
                              TransformUtil.substParams(transformedBody,params,tss.head).changeOwner(bodyOwner,meth).withSpan(body.span)
                           }
                          )
                          println(s"trasformDefDef: creation of nTpt")
                          val nTpt = AppliedType(tycon,CpsTransformHelper.adoptResultTypeParam(targs,monadType))
                          val defDef = cpy.DefDef(tree)(rhs=nRhs,tpt=TypeTree(nTpt))
                          println(s"transformDefDef result: ${defDef.show}")
                          defDef
                        catch
                          case ex:CpsTransformException =>
                            report.error(ex.message,ex.pos)
                            if (debug) {
                               ex.printStackTrace()
                            }
                            tree
                      case _ =>
                        println(s"returning context function with quotre but not a lambda")
                        super.transformDefDef(tree)
                  case head::tail =>
                    // TODO: find monad and implements compound algebraic effect
                    report.error(s"more than one CpsTransform argument:  ${head._1.show} and ${tail.head._1.show}", tree.srcPos)
                    tree
              case _  =>
                //println(s"not AppliedType")
                super.transformDefDef(tree)
          case _ =>
            println(s"is not applied-type=tree but ${tree.tpt} ")
            super.transformDefDef(tree)
    else
      super.transformDefDef(tree)    
  }

  override def transformApply(tree: Apply)(using Context):Tree = {
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
                      val tc = TransformationContext(monadType,am,params(0),optRuntimeAwait)
                      val cpsTree = RootTransform(body,bodyOwner,tc)
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
                    println(s"origin cpsAwait tree: ${tree.show}")
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


  private def revListCpsTransformInContextFunctionArgTypes(args:List[Type])(using Context):List[(Type,Int)] = {
      val cpsMonadContextSym = Symbols.requiredClass("cps.CpsMonadContext")
      revListCpsTransformInContextFunctionArgsTypesAcc(args,Nil,0,cpsMonadContextSym)
  }


  @tailrec
  private def revListCpsTransformInContextFunctionArgsTypesAcc(args:List[Type],acc:List[(Type,Int)],index:Int,cpsMonadContextSym:ClassSymbol)(using Context):List[(Type,Int)] = {
    args match
      case last::Nil => acc
      case Nil => acc
      case h::t =>
        h match
          case AppliedType(tycon, targs) if (tycon.typeSymbol == cpsMonadContextSym) => 
            // short way
            revListCpsTransformInContextFunctionArgsTypesAcc(t, (h,index)::acc, index+1, cpsMonadContextSym)
          case _ =>
            if (h <:< cpsMonadContextSym.typeRef.appliedTo(TypeBounds.empty)) {
              println("h <:< CpsMonadContext[_]")
              revListCpsTransformInContextFunctionArgsTypesAcc(t, (h,index)::acc, index+1, cpsMonadContextSym)
            } else {
              revListCpsTransformInContextFunctionArgsTypesAcc(t, acc, index+1, cpsMonadContextSym)
            }
  }



}

