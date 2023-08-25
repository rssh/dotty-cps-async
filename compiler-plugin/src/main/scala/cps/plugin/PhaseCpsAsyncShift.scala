package cps.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees
import core.*
import core.Names.*
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
  override val runsBefore           = Set(Erasure.name, PhaseCpsAsyncReplace.name)

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
    println(s"cpsAsyncShift::transformTemplate: ${tree.symbol.name}, ${tree.tpe.show}")
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
          // create PolyType for a new Symbol info
          val args = fun.paramss
          val typeArgs:   List[TypeDef]  = filterParams[TypeDef](args)
          val normalArgs: List[ValDef]   = filterParams[ValDef](args)
          // TODO: params into TermName
          val paramNames: List[TermName] = normalArgs.map(_.name)
          // create new return type with type params
          val newSymbolInfo  =
            PolyType(List("F".toTypeName, "C".toTypeName))(
              // bounds for the type parameters
              pt =>
                List(
                  TypeBounds(defn.NothingType, defn.AnyType),
                  TypeBounds(
                    defn.NothingType,
                    AppliedType(
                      Symbols
                        .requiredClassRef("cps.plugin.CpsMonadContext"),
                      List(pt.newParamRef(0))
                    )
                  )
                ),
              pt => {
                val mtParamTypes = List(
                  AppliedType(
                    TypeRef(
                      Symbols.requiredClassRef("cps.plugin.CpsMonad"),
                      "Aux".toTermName
                    ),
                    List(pt.newParamRef(0), pt.newParamRef(1))
                  )
                ) ++ normalArgs.map(_.tpt.tpe)
                val mtReturnType = pt.newParamRef(0).appliedTo(fun.symbol.info.widen)
                MethodType("am".toTermName :: paramNames)(
                  _ => mtParamTypes,
                  _ => mtReturnType
                )
                // MethodType(List("am".toTermName))
                // (
                // // params info expression
                // (mt: MethodType) =>
                //   List(
                //     AppliedType(
                //       TypeRef(
                //         Symbols.requiredClassRef("cps.plugin.CpsMonad"),
                //         "Aux".toTermName
                //       ),
                //       List(pt.newParamRef(0), pt.newParamRef(1))
                //     )
                //   ),
                // result type expression
                // (mt: MethodType) =>
                //   MethodType(paramNames)(
                //     mtn =>
                //       normalArgs.map(p =>
                //         p match
                //           case v: ValDef => v.tpt.tpe
                //           case d: TypeDef => d.rhs.tpe
                //       ),
                //     mtn => pt.newParamRef(0).appliedTo(fun.symbol.info.widen)
                // )
              }
            )
          val newFunName     = (fun.symbol.name.debugString + "$cps").toTermName
          val newFunSymbol   =
            Symbols.newSymbol(
              fun.symbol.owner,
              newFunName,
              fun.symbol.flags | Flags.Synthetic,
              newSymbolInfo
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
      println(
        "cpsAsyncShift::transformTemplate: added new methods: " + newMethods
          .map(_.name)
          .mkString(",")
      )
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
  //   example:
  //   map[A,B](collection:List[A])(f: A => B): List[B] =  doSpmetjong
  //   map[F[_],C <: CpsMonadContext[F],A,B](am: CpsMonad.Aux[F,C])(collection:List[A])(f: A => B): F[List[B]] = {
  //      cpsAsyncApply[F,List[B],C](am,
  //           mt = ContextMethodType(....)
  //           Lambda(mt, tss => transfomrdBody(f) )
  //      )
  //   }

  //   tranfromedBody(f) = {
  //      Apply(f, ...)  =>  await[[F,result-typr-of-apply,F]](f, ....)
  //      or throw unimplemented instead await
  //

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
    val valDefs: List[ValDef] = filterParams[ValDef](tree.paramss)
    val funcParams = valDefs.filter(p => isFunc(p.tpt.tpe))
    val retval     =
      if funcParams.nonEmpty then true
    // check the return type
      else
    // TODO: write implementation for this special case
    if isFunc(tree.rhs.tpe.finalResultType) then
      throw CpsTransformException(
        "Unsupported type of function. The return type must not be a function",
        tree.srcPos
      )
    else false

    retval

  def filterParams[T](params: List[ParamClause]): List[T] =
    val ps = params.flatten[ValDef | TypeDef]
    ps.collect { case v: T => v }

  def isFunc(t: Type)(using Context): Boolean =
    t match
      case _: AppliedType
          if (defn.isFunctionType(t) ||
            defn.isContextFunctionType(t)) =>
        true
      case _: MethodType => true
      case _: PolyType => true
      case _ => false

}

object PhaseCpsAsyncShift {
  val name: String = "rssh.cpsAsyncShift"
}
