package cps.plugin

import dotty.tools.dotc.*
import core.{Types, *}
import core.Contexts.*
import core.Constants.*
import core.Annotations.*
import core.Decorators.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Types.TypeRef
import plugins.*
import transform.{Erasure, Inlining, Pickler, PruneErasedDefs}

//TODO: merge with phaseSelect
class PhaseCpsAsyncShift(selectedNodes: SelectedNodes, shiftedSymbols: ShiftedSymbols)
    extends PluginPhase {

  override val phaseName = PhaseCpsAsyncShift.name

  // strange -
  override def allowsImplicitSearch = true
  override val runsAfter            = Set(PhaseCps.name)
  override val runsBefore           = Set(PhaseCpsAsyncReplace.name, Erasure.name)

  // override def run(using Context): Unit = {
  // TODO:
  //  check what async-shift needed in the current compilation unit
  //   -- generate one in the special class
  //   -- update the global cache, setting as key - signature of function or method, value - async tree
  // }


  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree =
    try
      transformTemplateInternal(tree)
    catch
      case ex: CpsTransformException =>
        report.error(ex.getMessage, ex.pos)
        tree

  /**
   * looks for annotated functions, changes them and add to shiftedSymbols
   * @param tree
   * @param Context
   * @return
   */
  def transformTemplateInternal(tree: Template)(using Context): Tree = {
    val annotationClass = Symbols.requiredClass("cps.plugin.annotation.makeCPS")
    var newMethods      = List.empty[DefDef]
    for (
      bodyTree <- tree.body
      if bodyTree.symbol.is(Flags.Method) /*&& generateCps */
    )
      bodyTree match
        case fun: DefDef
            if (!fun.symbol.isAnonymousFunction &&
              !fun.symbol.denot.getAnnotation(annotationClass).isEmpty) =>
          checkApplicableForMakeCPS(fun) match
            case Left(err) =>
              throw CpsTransformException(err, bodyTree.srcPos)
            case Right(_) =>
          val newFunName     = (fun.symbol.name.debugString + "$cps").toTermName


          val newFunSymbol   =
            Symbols.newSymbol(
              fun.symbol.owner,
              newFunName,
              fun.symbol.flags | Flags.Synthetic,
              fun.symbol.info.widen, // let be the same type for now
              //  TODO:  create PolyType with type-params and MethodType instead fun.symbol.info.widen
              //PolyType()
            )
          // create new rhs
          val transformedRhs = transformFunсBody(fun.rhs)
          val newMethod      =
            DefDef(
              newFunSymbol,
              // create new paramss
              newParamss =>
                TransformUtil
                  .substParams(
                    transformedRhs,
                    filterParams(fun.paramss),
                    newParamss.flatten
                  )
                  .changeOwner(fun.symbol, newFunSymbol)
            )
          shiftedSymbols.addAsyncShift(fun.symbol, newMethod)
          newMethods = newMethod :: newMethods
        case _ => ()

    val retval = if (newMethods.isEmpty) {
      super.transformTemplate(tree)
    } else {
      cpy.Template(tree)(body = tree.body ++ newMethods)
    }
    retval
  }


  // Hight-level description of generation of async-shofted version of dunction
  //  val typeParams = if (haveTypeParams) paramss.head else Nil
  //  val normalArgs = if (haveTypeParams) args.tail else args
  //  Apply(
  //    TypeApply(
  //          ref(Symbols.requiredMetod("cps.cpsAsyncApply")),
  //          List[
  //            task - proint amd look.
  //            TypeTree(F[_],TypeParam,),
  //          ]
  //    ),
  //      Tadk:  add parameter to alreadu existing function.
  //    List(
  //      ValDef(am, TypeTree(,,,)),
  //      ValDef(f, TypeTree(AppliedType(defn.ContextFunctionSymbol,))
  //    )
  //  )
  //
  //   map[A,B](collection:List[A])(f: A => B): List[B] =  doSpmetjong
  //   map$cps[F[_],C <: CpsMonadContext[F],A,B](am: CpsMonad.Aux[F,C])(collection:List[A])(f: A => B): F[List[B]] = {
  //      cpsAsyncApply[F,List[B],C](am,
  //           mt = ContextMethodType(....)
  //           Lambda(mt, tss => transfomrdBody(f) )
  //      )
  //   }
  //
  //   add type-params
  //      new typr of DefDef:  if its PolyType, - copy PolyType and add type parameter
  //                           MethodType - create PolyType with type-params and MethodType

  //   tranfromedBody(f) = {
  //      Apply(f, ...)  =>  await[[F,result-typr-of-apply,F]](f, ....)
  //
  //
  //  ???

  //  DefDef( ....   rhs = cpsAsyncShift ....  )
  //

  /**
   * transform rhs of the annotated function
   * @param tree
   * @param Context
   * @return
   */
  def transformFunсBody(tree: Tree)(using Context): Tree =
    // val finalResType = tree.tpe.finalResultType
    // if isFunc(finalResType) then transformInnerFunction(tree)
    // else
    tree
      .select(defn.String_+)
      .appliedTo(Literal(Constant("transformed")))

  /**
   * transform a function which is returned from the high-order annotated
   * function
   */
  def transformInnerFunction(tree: Tree)(using Context): Tree =
    tree match
      // TODO: create a check for inline function
      case Block((innerFunc: DefDef) :: Nil, expr) => // unpack inner function
        Block(List(transformInnerFunction(innerFunc)), expr)
      case t: DefDef => // create a transformed copy of original inner function
        val rhs            = t.rhs
        val transformedRHS = transformInnerFunction(rhs)
        cpy.DefDef(t)(t.name, t.paramss, t.tpt, transformedRHS)
      case Block(stats, expr: Apply) => // transform inner function
        val newExpr = transformFunсBody(expr)
        Block(stats, newExpr)

  def checkApplicableForMakeCPS(tree: DefDef)(using Context): Either[String,Unit] =
    // check ValDef input params
    if (!isHightOrderByArg(tree)) then
      Left("Object annotated with cps.plugin.annotation.makeCPS has to be a high-order function")
    else// check the return type
      if isFunc(tree.rhs.tpe.finalResultType) then
        Left("Unsupported type of function. The return type must not be a function")
      else
        Right(())

  def isHightOrderByArg(tree: DefDef)(using Context): Boolean =
    // check ValDef input params
    val valDefs: List[ValDef] = filterParams(tree.paramss)
    val funcParams = valDefs.filter(p => isFunc(p.tpt.tpe))
    funcParams.nonEmpty



  def filterParams(params: List[ParamClause]): List[ValDef] =
    val ps = params.flatten[ValDef | TypeDef]
    ps.collect { case v: ValDef => v }

  def isFunc(t: Type)(using Context): Boolean =
    val retval = t match
      case _: AppliedType
          if (defn.isFunctionType(t) ||
            defn.isContextFunctionType(t)) =>
        true
      case _: MethodType => true
      case _: PolyType => true
      case _ => false
    retval

}

object PhaseCpsAsyncShift {
  val name: String = "rssh.cpsAsyncShift"
}
