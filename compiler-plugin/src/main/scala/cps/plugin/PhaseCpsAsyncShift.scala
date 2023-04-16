package cps.plugin


import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Constants.*
import core.Decorators.*
import ast.tpd.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Types.TypeRef
import plugins.*
import transform.{Inlining, Pickler, PruneErasedDefs}


class PhaseCpsAsyncShift(shiftedSymbols:ShiftedSymbols) extends PluginPhase {

  override val phaseName = "rssh.cpsAsyncShift"

  // strange -
  override def allowsImplicitSearch = true
  override val runsAfter  = Set("rssh.cps")
  override val runsBefore = Set(Inlining.name)

  //override def run(using Context): Unit = {
    // TODO:
    //  check what async-shift needed in the current compilation unit
    //   -- generate one in the special class
    //   -- update the global cache, setting as key - signature of function or method, value - async tree
  //}

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree = {
     //println(s"transformTemplate: ${tree.symbol.name}, ${tree.symbol.name.mangledString}, ${tree.symbol.name.debugString}")

     var newMethods = List.empty[DefDef]
     for(tree <- tree.body if tree.symbol.is(Flags.Method) /*&& isHightOrder() && generateCps */) {
       tree match
         case fun: DefDef if (!fun.symbol.isAnonymousFunction)  =>
           //println(s"  ${fun.symbol.name}   ${tree.symbol.fullName}")
           if (fun.symbol.name.debugString == "myFun") {
             // do something
             println("we see myFun, so let's add new method")
             val newFunName = (fun.symbol.name.debugString+"$cps").toTermName
             val newFunSymbol =
               Symbols.newSymbol(fun.symbol.owner, newFunName,
                                fun.symbol.flags|Flags.Synthetic,
                                fun.symbol.info, //let be the same type for now
                                )
              val paramSymbol = Symbols.newSymbol(newFunSymbol, "x".toTermName, Flags.Param, Symbols.defn.IntType)
              val newMethod = DefDef(
                newFunSymbol,
                List(List(paramSymbol)),
                fun.tpt.tpe,
                Block( Nil,
                  //Throw(New(Symbols.requiredClassRef("java.lang.RuntimeException"), List(Literal(Constant("not implemented")))))
                  Throw(New(Symbols.requiredClassRef("java.lang.RuntimeException"), List()))
                ).withSpan(fun.rhs.span)
              )
              newMethods = newMethod :: newMethods
           }
     }
     val retval = if (newMethods.isEmpty) {
       tree
     } else {
       //tree
       cpy.Template(tree)(body = tree.body ++ newMethods)
     }
     //super.transformTemplate(tree)
     //println(s"after CpsAsyncShift, retval: ${retval.show}")
     retval
  }

}

