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
  override val runsBefore           = Set(PhaseCpsAsyncReplace.name)

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
  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
    println(
      s"cpsAsyncShift::transformTemplate: ${tree.symbol.name}, ${tree.symbol.info.show}"
    )
    val annotationClass = Symbols.requiredClass("cps.plugin.annotation.makeCPS")
    var newMethods      = List.empty[DefDef]
    for (
      bodyTree <- tree.body
      if bodyTree.symbol.is(Flags.Method) /*&& isHightOrder() && generateCps */
    )
      bodyTree match
        case fun: DefDef
            if (!fun.symbol.isAnonymousFunction &&
              !fun.symbol.denot.getAnnotation(annotationClass).isEmpty) =>
          val newFunName     = (fun.symbol.name.debugString + "$cps").toTermName
          val newFunSymbol   =
            Symbols.newSymbol(
              fun.symbol.owner,
              newFunName,
              fun.symbol.flags | Flags.Synthetic,
              fun.symbol.info.widen // let be the same type for now
            )
          // create new rhs
          val ctx1: Context = summon[Context].withOwner(newFunSymbol)
          // TODO: write transformation of func body and tests
          println(s"rhs.tpe=${fun.rhs.tpe.show}")
          val transformedRhs = transformFunctionBody(fun.rhs)
          // val nRhs           = Block(Nil, transformedRhs)(using ctx1)
          val newMethod      =
            DefDef(
              newFunSymbol,
              // create new paramss
              // TODO: transform all paramss
              newParamss =>
                TransformUtil
                  .substParams(
                    transformedRhs,
                    fun.paramss.head.asInstanceOf[List[ValDef]],
                    newParamss.head
                  )
                  .changeOwner(fun.symbol, newFunSymbol)
            )
          shiftedSymbols.addAsyncShift(fun.symbol, newMethod)
          newMethods = newMethod :: newMethods
        case _ => super.transformTemplate(tree)

    val retval = if (newMethods.isEmpty) {
      tree
    } else {
      // tree
      println("cpsAsyncShift::transformTemplate: added new methods: " + newMethods.map(_.name).mkString(","))
      cpy.Template(tree)(body = tree.body ++ newMethods)
    }
    // println(s"after CpsAsyncShift, retval: ${retval.show}")
    retval
  }


  //Hight-level description of generation of async-shofted version of dunction
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

  def transformFunctionBody(tree: tpd.Tree)(using Context): tpd.Tree =
    println("transformFunctionBody: tree.symbol=${tree.symbol}, tree=${tree.show}")

    //Symbol = Definition
    //   ValDef,  DefDef, TypeDef, ClassDef, PackageDef...

    val transformed =
      tree
        .select(defn.String_+)
        .appliedTo(tpd.Literal(Constant("transformed")))
    transformed


}

object PhaseCpsAsyncShift {
  val name: String = "rssh.cpsAsyncShift"
}
