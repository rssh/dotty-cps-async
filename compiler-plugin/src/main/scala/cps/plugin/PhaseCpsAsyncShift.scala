package cps.plugin

import dotty.tools.dotc.*
import core.*
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
import transform.{ Erasure, Inlining, Pickler, PruneErasedDefs }

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

  /**
   * looks for annotated functions, changes them and add to shiftedSymbols
   * @param tree
   * @param Context
   * @return
   */
  override def transformTemplate(tree: Template)(using Context): Tree = {
    println(
      s"cpsAsyncShift::transformTemplate: ${tree.symbol.name}, ${tree.tpe.show}"
    )
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
          if !isHighOrder(fun) then
            throw CpsTransformException(
              "Object has to be a high-order function",
              bodyTree.srcPos
            )
          val newFunName     = (fun.symbol.name.debugString + "$cps").toTermName
          val newFunSymbol   =
            Symbols.newSymbol(
              fun.symbol.owner,
              newFunName,
              fun.symbol.flags | Flags.Synthetic,
              fun.symbol.info.widen // let be the same type for now
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
      println("cpsAsyncShift::transformTemplate: added new methods: " + newMethods.map(_.name).mkString(","))
      cpy.Template(tree)(body = tree.body ++ newMethods)
    }
    // println(s"after CpsAsyncShift, retval: ${retval.show}")
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
  //   map[F[_],C <: CpsMonadContext[F],A,B](am: CpsMonad.Aux[F,C])(collection:List[A])(f: A => B): F[List[B]] = {
  //      cpsAsyncApply[F,List[B],C](am,
  //           mt = ContextMethodType(....)
  //           Lambda(mt, tss => transfomrdBody(f) )
  //      )
  //   }

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

  def isHighOrder(tree: DefDef)(using Context): Boolean =
    // check ValDef input params
    val valDefs: List[ValDef] = filterParams(tree.paramss)
    val funcParams = valDefs.filter(p => isFunc(p.tpt.tpe))
    val retval = if funcParams.nonEmpty then
      true
    else// check the return type
      // TODO: write implementation for this special case
      if isFunc(tree.rhs.tpe.finalResultType) then
        throw CpsTransformException(
          "Unsupported type of function. The return type must not be a function",
          tree.srcPos
        )
      else false
    println("isHightOrder,  params: " + valDefs.map(_.tpt.tpe.show).mkString(",") + ", retval: " + retval)
    println(s"isHightOrder,  funcParams: ${funcParams.map(_.tpt.tpe.show).mkString(",")}")
    retval

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
    println("isFunc, t: " + t.show + ", retval: " + retval)
    retval

}

object PhaseCpsAsyncShift {
  val name: String = "rssh.cpsAsyncShift"
}
