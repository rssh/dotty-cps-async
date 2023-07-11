package cps.plugin.forest.application

import scala.annotation.tailrec
import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Decorators.*
import core.Names.*
import core.Symbols.*
import core.Types.*
import ast.*
import ast.tpd.*
import cps.plugin.*
import cps.plugin.forest.*
import dotty.tools.dotc.ast.tpd

import scala.util.control.NonFatal

enum ApplyArgCallMode {
  case SYNC, ASYNC, ASYNC_SHIFT
}

sealed trait ApplyArg {
    def name: TermName
    def tpe:  Type
    def origin: Tree

    def isAsync(using Context, CpsTopLevelContext): Boolean
    def isLambda(using Context, CpsTopLevelContext): Boolean
    def isAsyncLambda(using Context, CpsTopLevelContext): Boolean
    def isDirectContext: Boolean

    def flatMapsBeforeCall(using Context): Seq[(CpsTree,ValDef)]
    def exprInCall(callMode: ApplyArgCallMode, optRuntimeAwait:Option[Tree])(using Context, CpsTopLevelContext): Tree

    //def dependencyFromLeft: Boolean
    def show(using Context): String
}

object ApplyArg {


  def apply(expr: Tree, paramName: TermName, paramType: Type,
            isByName: Boolean, isDirectContext: Boolean,
            owner: Symbol,
            dependFromLeft: Boolean, nesting: Int)(using Context, CpsTopLevelContext): ApplyArg = {
    val retval = expr match
      case Typed(sq@SeqLiteral(elems,elemtpt),rtp) if isRepeatedParamType(rtp) =>
        RepeatApplyArg(paramName, paramType, elems.zipWithIndex.map{ (p,i) =>
          val newName = (paramName.toString + i.toString).toTermName
          ApplyArg(p,newName,elemtpt.tpe,isByName, isDirectContext, owner, dependFromLeft,  nesting)
        },  elemtpt,  expr)
      case _ =>
        val cpsExpr = try{
          RootTransform(expr, owner, nesting+1)
        }catch {
          case NonFatal(ex) =>
            expr match
              case Typed(expr,tpt) =>
                println(s"Failed term: ${expr}")
                println(s"Failed type: ${tpt}")
              case _ =>
                println(s"Failed term: ${expr}")
            throw ex
        }
        Log.trace(s"ApplyArg: ${expr.show} => ${cpsExpr.show}", nesting)
        if (isByName) then
          ByNameApplyArg(paramName, paramType, cpsExpr, isDirectContext)
        else
          paramType match
            case AnnotatedType(tp, an) if an.symbol == defn.InlineParamAnnot =>
                InlineApplyArg(paramName,tp,cpsExpr,isDirectContext)
            case AnnotatedType(tp, an) if an.symbol == defn.ErasedParamAnnot =>
                ErasedApplyArg(paramName,tp,expr,isDirectContext)
            case _ =>
                cpsExpr.asyncKind match
                  case AsyncKind.Sync if !dependFromLeft =>
                    PlainApplyArg(paramName,paramType,cpsExpr,None,isDirectContext)
                  case AsyncKind.AsyncLambda(_) =>

                    PlainApplyArg(paramName,paramType,cpsExpr,None,isDirectContext)
                  case _ =>
                    val sym = newSymbol(owner,paramName,Flags.EmptyFlags,paramType.widen,NoSymbol)
                    val optRhs =  cpsExpr.unpure
                    val valDef =  ValDef(sym.asTerm, optRhs.getOrElse(EmptyTree).changeOwner(cpsExpr.owner, sym))
                    PlainApplyArg(paramName,paramType.widen,cpsExpr,Some(valDef), isDirectContext)
    Log.trace(s"creating arg for expr: ${expr.show}, resut=${retval.show}", nesting)
    retval
  }

}

sealed trait ExprApplyArg extends ApplyArg {

  def expr: CpsTree

  override def origin: tpd.Tree = expr.origin

  override def isAsync(using Context, CpsTopLevelContext) = expr.asyncKind match
                            case AsyncKind.Async(_) => true
                            case _ =>false

  override def isLambda(using Context, CpsTopLevelContext) =
    expr.asyncKind match
      case AsyncKind.AsyncLambda(internal) => true
      case _ => false


  override def isAsyncLambda(using Context, CpsTopLevelContext): Boolean = expr.asyncKind match
    case AsyncKind.AsyncLambda(internal) =>
      checkInternalAsyncLambda(internal) && !lambdaCanBeUnshifted
    case _ => false

  def lambdaCanBeUnshifted(using Context, CpsTopLevelContext): Boolean = {

    def isAsync(tp: Type): Boolean = {
      tp.baseType(summon[CpsTopLevelContext].monadType.typeSymbol) != NoType
    }

    @tailrec
    def canBeUnshifted(tp: Type): Boolean = {
      tp match
        case tp: MethodOrPoly => isAsync(tp.resType) || canBeUnshifted(tp.resType)
        case AppliedType(tycon, targs) if defn.isFunctionType(tycon) =>
          val tp = targs.last
          isAsync(tp) || canBeUnshifted(tp)
        case _ => false
    }

    canBeUnshifted(tpe.widen)

  }


  def checkInternalAsyncLambda(kind: AsyncKind): Boolean =
    kind match
      case AsyncKind.AsyncLambda(internal) =>
         checkInternalAsyncLambda(internal)
      case AsyncKind.Async(internal) => true
      case AsyncKind.Sync => false


}


case class PlainApplyArg(  
  override val name: TermName,
  override val tpe: Type,
  override val expr: CpsTree,  
  val optIdentValDef: Option[ValDef],
  val isDirectContext: Boolean,
) extends ExprApplyArg  {

  override def flatMapsBeforeCall(using Context): Seq[(CpsTree,ValDef)] = {
    optIdentValDef.map(valDef => (expr,valDef)).toSeq
  }

  /**
   *  Output the expression inside call
   *    this can 
   *
   *  //Are we change symbol when do changeOwner to tree ?
   *  If yes, we should be extremally careful with different refs to
   *  optIdentSym.  Maybe better do expr function from sym ?
   *
   *  TODO:  dependFromLeft
   **/
   override def exprInCall(callMode: ApplyArgCallMode, optRuntimeAwait:Option[Tree])(using Context, CpsTopLevelContext): Tree =
    import AsyncKind.*
    expr.asyncKind match
      case Sync => expr.unpure match
        case Some(tree) =>
          tree
        case None => throw CpsTransformException("Impossibke: syn expression without unpure",expr.origin.srcPos)
      case Async(_) => ref(optIdentValDef.get.symbol)
      case AsyncLambda(internal) =>
        if (callMode == ApplyArgCallMode.ASYNC_SHIFT) then
          expr.transformed
        else
          expr.unpure match
            case Some(tree) => tree
            case None =>
              if (lambdaCanBeUnshifted) then
                unshiftLambdaAsTree
              else
                optRuntimeAwait match
                  case Some(runtimeAwait) =>
                    val withRuntimeAwait = expr.applyRuntimeAwait(runtimeAwait)
                    withRuntimeAwait.unpure match
                      case Some(tree) => tree
                      case None =>
                        throw CpsTransformException(s"Can't transform function via RuntimeAwait",expr.origin.srcPos)
                  case None =>
                    throw CpsTransformException(s"Can't transform function (both shioft and runtime-awaif for ${summon[CpsTopLevelContext].monadType} are not found)", expr.origin.srcPos)


  override def show(using Context): String = {
    s"Plain(${expr.show})"
  }


  def unshiftLambda(expr:CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    expr match
      case LambdaCpsTree(origin,owner,originDefDef, cpsBody) =>
        //cpsBody.unpure match
        //  case Some(tree) =>
        //    val nCpsBody = CpsTree.impure(origin,owner,tree,AsyncKind.Sync)
        //    LambdaCpsTree(origin,owner,originDefDef,nCpsBody)
        /*
        val unwrapped = CpsTransformHelper.unwrappTypeFromMonad(tpe, summon[CpsTopLevelContext].monadType)
        val nCpsBody = Apply(
                         TypeApply(
                           Select(summon[CpsTopLevelContext].cpsMonadRef,"flatten".toTermName),
                           List(TypeTree(unwrapped))
                         ),
                         List(cpsBody.transformed)
        )
        */
        val tctx = summon[CpsTopLevelContext]
        val monadRef = tctx.cpsMonadRef
        val nCpsBodyTree = ctx.typer.typed(untpd.Apply(
          untpd.Select(untpd.TypedSplice(monadRef), "flatten".toTermName),
          List( untpd.TypedSplice(cpsBody.transformed) )
        ))
        val nCpsBody = CpsTree.impure(origin,owner,nCpsBodyTree,expr.asyncKind)
        val newLambda = LambdaCpsTree(origin,owner,originDefDef, nCpsBody)
        newLambda
      case opl@OpaqueAsyncLambdaTermCpsTree(origin, owner, transformedTree, bodyKind) =>
        unshiftLambda(opl.toLambdaCpsTree)
      case BlockBoundsCpsTree(internal) =>
        // TODO: eta-expansion
        BlockBoundsCpsTree(unshiftLambda(internal))
      case SeqCpsTree(origin,owner,prevs,last) =>
        SeqCpsTree(origin,owner,prevs,unshiftLambda(last))
      case _ =>
        // remaining CpsTrees are not lambda, so we can't unshift them.
        throw CpsTransformException(s"Can't unshift lambda from ${expr.show}", expr.origin.srcPos)
  }

  /**
   * prerequisities:  kind == AsyncLambda
   * @param Context
   * @param CpsTopLevelContext
   * @return
   */
  def unshiftLambdaAsTree(using Context, CpsTopLevelContext): Tree = {

    def applyFlatten(tree: Tree): Tree = {
        val tctx = summon[CpsTopLevelContext]
        val monadRef = tctx.cpsMonadRef
        val nTree = summon[Context].typer.typed(untpd.Apply(
          untpd.Select(untpd.TypedSplice(monadRef), "flatten".toTermName),
          List(untpd.TypedSplice(tree))
        ))
        nTree
    }

    expr match
        case LambdaCpsTree(origin, owner, originDefDef, cpsBody) =>
          val meth = Symbols.newAnonFun(owner,originDefDef.tpe.widen)
          val nBody = applyFlatten(cpsBody.transformed)
          val closure = Closure(meth, { tss =>
            val oldParams = originDefDef.paramss.head.asInstanceOf[List[ValDef]]
            TransformUtil.substParams(nBody,oldParams,tss.head).changeOwner(originDefDef.symbol,meth)
          })
          closure
        case _ =>
          val lambdaTree = expr.transformed
          TransformUtil.methodTypeFromFunctionType(tpe,origin.srcPos) match
            case Some(mt) =>
              val meth = Symbols.newAnonFun(expr.owner,mt)
              val ctx = summon[Context]
              val closure = Closure(meth, { tss =>
                 given Context = ctx.withOwner(meth)
                 val unflattenCall = Apply(lambdaTree,tss.head)
                 applyFlatten(unflattenCall).changeOwner(expr.owner,meth)
              })
              closure
            case None =>
              throw CpsTransformException(s"Can't extract methd type from ${tpe.show}", expr.origin.srcPos)

  }


}

case class RepeatApplyArg(
  override val name: TermName,
  override val tpe: Type,
  elements: Seq[ApplyArg],
  elementTpt: Tree,
  override val origin: Tree,
) extends ApplyArg {

  override def isAsync(using Context, CpsTopLevelContext) = elements.exists(_.isAsync)

  override def isLambda(using Context, CpsTopLevelContext) = elements.exists(_.isLambda)

  override def isAsyncLambda(using Context, CpsTopLevelContext): Boolean = elements.exists(_.isAsyncLambda)

  override def isDirectContext: Boolean = elements.exists(_.isDirectContext)


  override def flatMapsBeforeCall(using Context) = 
    elements.foldLeft(IndexedSeq.empty[(CpsTree,ValDef)]){ (s,e) =>
       s ++ e.flatMapsBeforeCall
    }

  override def exprInCall(callMode: ApplyArgCallMode, optRuntimeAwait:Option[Tree])(using Context, CpsTopLevelContext) =
    val trees = elements.foldLeft(IndexedSeq.empty[Tree]){ (s,e) =>
      s.appended(e.exprInCall(callMode,optRuntimeAwait))
    }
    val (nElemTpt, nRtp) = callMode match
      case ApplyArgCallMode.ASYNC_SHIFT if elements.exists(_.isLambda) =>
        val elemTpe = CpsTransformHelper.cpsTransformedType(elementTpt.tpe, summon[CpsTopLevelContext].monadType)
        val elemTpt = TypeTree(elemTpe)
        (elemTpt, AppliedTypeTree(TypeTree(defn.RepeatedParamType), List(elemTpt)))
      case _ => (elementTpt, TypeTree(tpe))
    // todo - return orign if nothing was changed
    Typed(SeqLiteral(trees.toList,nElemTpt),nRtp).withSpan(origin.span)

  override def show(using Context): String = {
    s"Repeated(${elements.map(_.show)})"
  }

}

case class ByNameApplyArg(
  override val name: TermName,
  override val tpe: Type,
  override val expr: CpsTree,
  override val isDirectContext: Boolean,
) extends ExprApplyArg  {

  override def isLambda(using Context, CpsTopLevelContext) = true

  override def isAsyncLambda(using Context, CpsTopLevelContext): Boolean = expr.asyncKind != AsyncKind.Sync

  override def flatMapsBeforeCall(using Context) = Seq.empty

  override def exprInCall(callMode: ApplyArgCallMode, optRuntimeAwait:Option[Tree])(using Context, CpsTopLevelContext): Tree = {
    callMode match
      case ApplyArgCallMode.ASYNC_SHIFT =>
        // make lambda
        val mt = MethodType(List())(
          x => List(),
          x => CpsTransformHelper.cpsTransformedType(expr.originType.widen, summon[CpsTopLevelContext].monadType)
        )    
        val meth = Symbols.newAnonFun(expr.owner,mt)
        val nArg = Closure(meth,tss => {
          expr.transformed.changeOwner(expr.owner,meth).withSpan(expr.origin.span)
        })
        nArg
      case _ =>
        expr.unpure match
          case Some(tree) => tree
          case None => 
            optRuntimeAwait match
              case Some(runtimeAwait) => 
                expr.applyRuntimeAwait(runtimeAwait).unpure match
                  case Some(tree) => tree
                  case None =>
                    throw CpsTransformException("Invalid result of RuntimeAwait",expr.origin.srcPos)
              case None =>
                throw CpsTransformException("Can't trandform arg call to sync form without runtimeAwait",expr.origin.srcPos)   
  }

  override def show(using Context): String = {
    s"ByName(${expr.origin.show})"
  }

}



// potentially not used if we run after macros,
// but we can return 
case class InlineApplyArg(
  override val name: TermName,
  override val tpe: Type,
  override val expr: CpsTree,
  override val isDirectContext: Boolean
) extends ExprApplyArg {


  override def isAsync(using Context, CpsTopLevelContext): Boolean =
    throwNotHere
     
  override def isLambda(using Context, CpsTopLevelContext): Boolean =
    throwNotHere

  def flatMapsBeforeCall(using Context): Seq[(CpsTree,ValDef)] =
    throwNotHere

  def exprInCall(callMode: ApplyArgCallMode, optRuntimeAwait:Option[Tree])(using Context, CpsTopLevelContext): Tree =
    throwNotHere

  def throwNotHere: Nothing =
    throw new CpsTransformException("Inlined parameter should be erased on previous phase",expr.origin.srcPos)

  override def show(using Context): String = {
    s"Inline(${expr.origin.show})"
  }


}

// erased 
case class ErasedApplyArg(
  override val name: TermName,
  override val tpe: Type,
           val exprTree: Tree,
  override val isDirectContext: Boolean
) extends ApplyArg {

  override def origin: tpd.Tree = exprTree

  override def isAsync(using Context, CpsTopLevelContext): Boolean =
    false
   
  override def isLambda(using Context, CpsTopLevelContext): Boolean =
    false

  override def isAsyncLambda(using Context, CpsTopLevelContext): Boolean =
     false

  def flatMapsBeforeCall(using Context): Seq[(CpsTree,ValDef)] =
    Seq.empty

  def exprInCall(callMode: ApplyArgCallMode, optRuntimeAwait:Option[Tree])(using Context, CpsTopLevelContext): Tree =
    exprTree

  override def show(using Context): String = {
    s"Erased(${exprTree.show})"
  }
  

}
