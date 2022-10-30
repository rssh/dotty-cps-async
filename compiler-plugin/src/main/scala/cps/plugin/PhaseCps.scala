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

  val phaseName = "cps"

  override val runsAfter = Set("cc")
  override val runsBefore = Set("cpsAsyncShift")

  val debug = true


  override def transformDefDef(tree:DefDef)(using Context): Tree = {
    // TODO:
    //  find parameter with outer value CpsTransform[F] ?=> T
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
                  case (cpsTransformType, argIndex)::Nil =>
                    tree.rhs match
                      case CheckLambda(params, body, bodyOwner) =>
                        try 
                          println(s"lambda found as resulting expression, params=$params")
                          val monadType = CpsTransformHelper.extractMonadType(cpsTransformType,tree.srcPos)
                          val cpsTransformParam = params(argIndex)
                          val monadFieldName = "m".toTermName
                          val monad = Select(cpsTransformParam, monadFieldName).withSpan(tree.span)
                          //val oldMt = tree.rhs.tpe match
                          //  case v:MethodType => v
                          //  case _  => throw CpsTransformException(s"type of context function is not MethodType but ${tree.rhs.tpe}",tree.srcPos)
                          val mt = CpsTransformHelper.transformContextualLambdaType(tree.rhs,params,body,monadType)
                          val meth = Symbols.newAnonFun(summon[Context].owner,mt)
                          val nRhs = Closure(meth,tss => {
                              val tc = TransformationContext(monadType,monad)
                              val cpsTree = RootTransform(body,tc)
                              val transformedBody = cpsTree.transformed
                              TransformUtil.substParams(transformedBody,params,tss.head).changeOwner(bodyOwner,meth).withSpan(body.span)
                           }
                          )
                          val nTpt = AppliedType(tycon,CpsTransformHelper.adoptResultTypeParam(targs,mt))
                          val defDef = cpy.DefDef(tree)(rhs=nRhs,tpt=TypeTree(nTpt))
                          println(s"result: ${defDef.show}")
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
                if tycon.typeSymbol == Symbols.requiredClass("cps.E.CpsTransform.InfernAsyncArg") =>
                  println(s"found CpsAsync candidate, tycon.typeSymbol=:  ${tycon.typeSymbol} ")
                  try
                    val cpsTransformType = params(0).tpt.tpe
                    val monadType = CpsTransformHelper.extractMonadType(cpsTransformType,tree.srcPos)
                    val mt = MethodType(List(params(1).name))(
                      x => List(params(1).tpt.tpe),
                      x => tree.tpe.widen  //decorateTypeApplications(monadType).appliedTo(body.tpe)
                    )
                    // TODO:  pass am to apply 
                    val am = Select(infernAsyncArgCn,"am".toTermName)
                    val meth = Symbols.newAnonFun(summon[Context].owner,mt)
                    val ctxFun = Closure(meth, tss => {
                      val tc = TransformationContext(monadType,am)
                      val cpsTree = RootTransform(body,tc)
                      val transformedBody = cpsTree.transformed
                      TransformUtil.substParams(transformedBody,List(params(1)),tss.head)
                                    .changeOwner(bodyOwner,meth)
                                    .withSpan(body.span)
                    }) 
                    val apply = Apply(
                                TypeApply(Select(am,"apply".toTermName),List(TypeTree(body.tpe.widen))),
                                List(ctxFun)
                    ).withSpan(tree.span)
                    apply
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
      val classSym = Symbols.requiredClass("cps.E.CpsTransform")
      revListCpsTransformInContextFunctionArgsTypesAcc(args,Nil,0,classSym)
  }


  @tailrec
  private def revListCpsTransformInContextFunctionArgsTypesAcc(args:List[Type],acc:List[(Type,Int)],index:Int,classSym:ClassSymbol)(using Context):List[(Type,Int)] = {
    args match
      case last::Nil => acc
      case Nil => acc
      case h::t =>
        h match
          case AppliedType(tycon, targs) if (tycon.typeSymbol == classSym) => 
            revListCpsTransformInContextFunctionArgsTypesAcc(t, (h,index)::acc, index+1, classSym)
          case _ =>
            revListCpsTransformInContextFunctionArgsTypesAcc(t, acc, index+1, classSym)
  }


}

