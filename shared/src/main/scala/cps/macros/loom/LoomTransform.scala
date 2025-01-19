package cps.macros.loom

import scala.quoted.*
import scala.util.control.NonFatal

import cps.*
import cps.macros.*
import cps.macros.misc.*
import cps.macros.common.*

// LoomTransform substitute all awaits in text
//  Note, that this macros can be potentially eliminated completely, by makeing
// hight-level await a macro call. The price will be possible complexity of await catching,
//  via yet one inline.
object LoomTransform:

  def run[F[_]: Type, T: Type, C <: CpsTryMonadContext[F]: Type](
      f: Expr[T],
      dm: Expr[CpsAsyncMonad[F]],
      ctx: Expr[C],
      runtimeApi: Expr[CpsRuntimeAwait[F]],
      flags: AsyncMacroFlags
  )(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val awaitSymbol = Symbol.requiredMethod("cps.await")

    val needVarTransformationForAutomaticColoring: Boolean = false

    val treeMap = new TreeMap() {

      override def transformTerm(term: Term)(owner: Symbol): Term = {
        if flags.debugLevel >= 20 then
          log(s"loom:transformTerm start, term=${term.show}")
          log(s"loom:transformTerm start, tree=${term}")
        term match
          case applyTerm @ Apply(fun, args) =>
            fun match
              case funApply @ Apply(fun1 @ TypeApply(obj2, targs2), args1) if obj2.symbol == awaitSymbol =>
                // catch await early
                val (awaitable, monadContext) = args match
                  case List(frs, snd) => (frs, snd)
                  case other =>
                    throw MacroError(s"expected that await have two implicit argument, our args:${args}", applyTerm.asExpr)
                runAwait(applyTerm, args1.head, targs2.head.tpe, awaitable, monadContext)(owner)
              case funApply @ Apply(fun1 @ TypeApply(obj2, targs2), args1) =>
                if flags.debugLevel >= 20 then
                  log(s"loom:transformTerm/applyTerm finish2 (noSyn), term=${term}, no await")
                  log(s"loom:transformTerm/applyTerm finish2 (noSyn), obj2=${obj2}")
                super.transformTerm(term)(owner)
              case Select(obj, method) =>
                handleFunSelect(applyTerm, fun, args, obj, method)(owner)
              case _ =>
                if flags.debugLevel >= 20 then log(s"loom:transformTerm/applyTerm finish1, term=${term}, no await")
                super.transformTerm(term)(owner)
          case _ =>
            if flags.debugLevel >= 20 then log(s"loom:transformTerm: call suoer of ${term}")
            super.transformTerm(term)(owner)
      }

      def handleFunSelect(applyTerm: Apply, fun: Term, args: List[Term], obj: Term, methodName: String)(owner: Symbol): Term = {
        obj match
          case conv @ Inlined(
                _,
                _,
                Typed(Lambda(List(xValDef), Block(List(), Apply(Apply(TypeApply(obj3, targs3), List(x)), args1))), cv)
              )
              if (obj3.symbol == awaitSymbol
                && xValDef.symbol == x.symbol) =>
            // here we catch await, inserted by implicit conversion.
            // this code is likey depends from implementation details of a compiler
            // mb create compiler-level API ?
            withInlineBindings(conv, runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head)(owner))
          case conv @ Inlined(_, _, Lambda(List(xValDef), Block(List(), Apply(Apply(TypeApply(obj3, targs3), List(x)), args1))))
              if (obj3.symbol == awaitSymbol
                && xValDef.symbol == x.symbol) =>
            // transient inlines have no 'Typed' entry
            //  TODO: handle non-inlined conversion
            withInlineBindings(conv, runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head)(owner))
          case _ =>
            super.transformTerm(applyTerm)(owner)
      }

      def withInlineBindings(origin: Inlined, tree: Term): Term =
        if (origin.bindings.isEmpty)
          tree
        else
          Inlined.copy(origin)(origin.call, origin.bindings, tree)

      def runAwait(term: Apply, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonad: Term, awaitCpsMonadContext: Term)(
          owner: Symbol
      ): Term = {
        if flags.debugLevel >= 10 then
          log(s"loom:runAwait, arg code=${arg.show}")
          log(s"loom:runAwait, arg tree=${arg}")
        val transformedArg = this.transformTerm(arg)(owner)
        val r =
          if awaitCpsMonadType <:< TypeRepr.of[F] then runMyAwait(term, transformedArg, awaitCpsMonadContext)
          else runOtherAwait(term, transformedArg, awaitCpsMonadType, awaitCpsMonad, awaitCpsMonadContext)
        if flags.debugLevel >= 10 then log(s"loom:runAwait, result=${r}")
        r
      }

      def runMyAwait(applyTerm: Apply, arg: Term, ctx: Term): Term = {
        Apply.copy(applyTerm)(
          Apply.copy(applyTerm)(
            TypeApply(Select.unique(runtimeApi.asTerm, "await"), List(Inferred(applyTerm.tpe.widen))),
            List(arg)
          ),
          List(dm.asTerm, ctx)
        )
      }

      def runOtherAwait(applyTerm: Apply, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonad: Term, ctx: Term): Term = {
        // TODO: set position of
        // Apply.copy(applyTerm)(Select(runtimeApi.asTerm,"await"), List(arg) )
        val myCpsMonad = dm
        val myCpsMonadTpe = dm.asTerm.tpe.widen
        val myF = TypeRepr.of[F]
        val tTpe = applyTerm.tpe.widen
        val monadConversion = TypeIdent(Symbol.classSymbol("cps.CpsMonadConversion")).tpe
        val taConversion = monadConversion.appliedTo(List(awaitCpsMonadType, TypeRepr.of[F]))
        Implicits.search(taConversion) match
          case implSuccess: ImplicitSearchSuccess =>
            // val convertedArg = Apply(Select.unique(implSuccess.tree, "apply"),List(arg))
            val convertedArg =
              Apply(TypeApply(Select.unique(implSuccess.tree, "apply"), List(Inferred(applyTerm.tpe.widen))), List(arg))
            runMyAwait(applyTerm, convertedArg, ctx)
          case implFailure: ImplicitSearchFailure =>
            val taConversionPrinted =
              try {
                taConversion.show
              } catch {
                case NonFatal(ex) =>
                  taConversion.toString
              }
            throw MacroError(s"Can't find ${taConversionPrinted}: ${implFailure.explanation}", applyTerm.asExpr)
      }

    }

    val retval = treeMap.transformTerm(f.asTerm)(Symbol.spliceOwner)
    retval.asExprOf[T]
  }

  def log(msg: => String)(using Quotes): Unit =
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion
    val fileName = pos.sourceFile.jpath.getFileName
    val line = pos.startLine + 1
    val col = pos.startColumn + 1
    println(s"[${fileName}:${line}:${col}]: ${msg}")

end LoomTransform
