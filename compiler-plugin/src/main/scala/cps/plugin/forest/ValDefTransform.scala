package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.*
import ast.tpd.*
import cps.plugin.*
import dotty.tools.dotc.ast.untpd.TypedSplice

object ValDefTransform {


      def apply(term: ValDef, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
            val tctx = summon[CpsTopLevelContext]
            if (term.rhs.isEmpty) then
                  throw CpsTransformException(s"ValDef without right part: $term", term.srcPos)
            val cpsRhs0 = RootTransform(term.rhs,term.symbol,nesting+1)
            val cpsRhs = tctx.automaticColoring match
                  case Some(c) if (cpsRhs0.originType <:< tctx.monadType.appliedTo(Types.WildcardType)) =>
                       c.analyzer.usageRecords.get(term.symbol) match
                             case Some(record) =>
                                   record.reportCases()
                                   if (record.nInAwaits > 0 && record.nWithoutAwaits == 0) {
                                     applyMemoization(cpsRhs0, owner, c.memoization, term)
                                   } else if (record.nInAwaits >0 && record.nWithoutAwaits > 0) {
                                     record.reportCases()
                                     throw CpsTransformException(s"val ${term.name} used in both sync and async way with automatic coloring", term.srcPos)
                                   } else {
                                     cpsRhs0
                                   }
                             case None => cpsRhs0
                  case _ => cpsRhs0
            cpsRhs.asyncKind match
                  case AsyncKind.Sync =>
                        if (cpsRhs.isOriginEqSync) then
                              CpsTree.unchangedPure(term,owner)
                        else
                              val newValDef = cpy.ValDef(term)(name=term.name, tpt=term.tpt, rhs=cpsRhs.unpure.get)
                              CpsTree.pure(term,owner,newValDef)
                  case AsyncKind.Async(_) =>
                        val mapCpsTreeArgument = if (true && term.symbol.flags.is(Flags.Mutable)) then
                              val paramSym = Symbols.newSymbol(owner, ("p"+term.name).toTermName, Flags.EmptyFlags, term.tpt.tpe.widen, coord = term.symbol.coord)
                              val paramValDef = ValDef(paramSym, EmptyTree)
                              val nValDef = cpy.ValDef(term)(term.name,term.tpt, ref(paramValDef.symbol).withSpan(term.span))
                              MapCpsTreeArgument(
                                    Some(paramValDef),
                                    MemberDefCpsTree(term,owner,nValDef)
                              )
                        else
                              val nValDef = cpy.ValDef(term)(term.name,term.tpt, EmptyTree)
                              MapCpsTreeArgument(
                                    Some(nValDef),
                                    CpsTree.unit(owner)
                              )

                        MapCpsTree(
                              term,
                              owner,
                              cpsRhs.changeOwner(owner),
                              mapCpsTreeArgument
                        )
                  case rhsFunJind: AsyncKind.AsyncLambda =>
                        // here value is a function.
                        cpsRhs.unpure match
                              case Some(rhs) =>
                                    if (cpsRhs.isOriginEqSync) then
                                          CpsTree.unchangedPure(term,owner)
                                    else
                                          val nValDef = cpy.ValDef(term)(term.name,term.tpt,rhs)
                                          CpsTree.pure(term,owner,nValDef)
                              case None =>
                                    tctx.optRuntimeAwait match
                                          case Some(runtimeAwait) =>
                                                val newLambda = cpsRhs.applyRuntimeAwait(runtimeAwait).unpure.get
                                                val nValDef = cpy.ValDef(term)(term.name,term.tpt,rhs=newLambda)
                                                CpsTree.pure(term,owner,nValDef)
                                          case None =>
                                                //we can't change types in plugin,
                                                // Theoretically it's possible to track usage of ValDef and fix xhanged,
                                                //  but let think about this after an initial release
                                                throw CpsTransformException(s"Functional variable trasnfrom is not supported",term.srcPos)


      }


      def applyMemoization(cpsRhs: CpsTree, owner: Symbol, memoization: Tree, term: ValDef)(using Context, CpsTopLevelContext): CpsTree = {
            if (memoization.tpe <:< Symbols.requiredClassRef("cps.CpsMonadMemoization.Default").appliedTo(Types.WildcardType)) then
                  cpsRhs
            else if (memoization.tpe.baseType(Symbols.requiredClass("cps.CpsMonadMemoization.Inplace")) != NoType) then
                  cpsRhs.unpure match
                    case Some(unpureRhs) =>
                      val untpdTerm = untpd.Apply(
                        untpd.Select(TypedSplice(memoization), "apply".toTermName).withSpan(memoization.span),
                        List(TypedSplice(cpsRhs.unpure.get))
                      ).withSpan(term.rhs.span)
                      val typedTerm = ctx.typer.typedExpr(untpdTerm)
                      CpsTree.pure(term, owner, typedTerm)
                    case None =>
                      cpsRhs.asyncKind match
                        case AsyncKind.Sync =>
                           throw CpsTransformException(s"Sync value ${term.name} used in both sync and async way with automatic coloring", term.srcPos)
                        case AsyncKind.Async(internalKind) =>
                          //  monad.map[F[T],F[T]](cpsRhs)(x => memoization.apply(x))
                          val toMemoizeSym = Symbols.newSymbol(owner, (term.name.toString+"$toMemoize").toTermName, Flags.EmptyFlags,term.tpt.tpe.widen, coord = term.symbol.coord)
                          val toMemoizeValDef = ValDef(toMemoizeSym, EmptyTree)
                          val typedBody = ctx.typer.typedExpr(
                            untpd.Apply(
                              untpd.Select(untpd.TypedSplice(memoization), "apply".toTermName).withSpan(memoization.span),
                              List(untpd.TypedSplice(ref(toMemoizeSym).withSpan(term.span)))
                            )
                          )
                          MapCpsTree(term,owner,cpsRhs,MapCpsTreeArgument(
                              Some(toMemoizeValDef),
                              CpsTree.pure(cpsRhs.origin, cpsRhs.owner, typedBody)
                          ))
                        case AsyncKind.AsyncLambda(bodyKind) =>
                          throw CpsTransformException(s"Can't apply inplace memoization to function ${term.name}", term.srcPos)
            else if (memoization.tpe.baseType(Symbols.requiredClass("cps.CpsMonadMemoization.Pure")) != NoType) then
              cpsRhs.unpure match
                case Some(unpureRhs) =>
                  val untpdTerm = untpd.Apply(
                    untpd.Select(TypedSplice(memoization), "apply".toTermName).withSpan(memoization.span),
                    List(TypedSplice(unpureRhs))
                  ).withSpan(term.rhs.span)
                  val typedTerm = ctx.typer.typedExpr(untpdTerm)
                  CpsTree.impure(term, cpsRhs.owner, typedTerm, AsyncKind.Sync)
                case None =>
                  cpsRhs.asyncKind match
                    case AsyncKind.Sync =>
                      throw CpsTransformException(s"Impossible (Sync asyncKind without unpure) for ${cpsRhs}", term.srcPos)
                    case AsyncKind.Async(internalKind) =>
                      //  monad.flatMap[F[T],F[T]](cpsRhs)(x => memoization.apply(x))]
                      val toMemoizeSym = Symbols.newSymbol(owner, "toMemoize".toTermName, Flags.EmptyFlags,term.tpt.tpe.widen, coord = term.symbol.coord)
                      val toMemoizeValDef = tpd.ValDef(toMemoizeSym, EmptyTree)
                      val memoizeBody = ctx.typer.typedExpr(
                        untpd.Apply(
                          untpd.Select(untpd.TypedSplice(memoization), "apply".toTermName),
                          List(untpd.TypedSplice(ref(toMemoizeSym).withSpan(term.span)))
                        ).withSpan(term.rhs.span)
                      )
                      FlatMapCpsTree(term,owner,cpsRhs,FlatMapCpsTreeArgument(
                        Some(toMemoizeValDef),
                        CpsTree.impure(cpsRhs.origin, cpsRhs.owner, memoizeBody, AsyncKind.Sync)
                      ))
                    case AsyncKind.AsyncLambda(bodyKind) =>
                      throw CpsTransformException(s"Can't apply inplace memoization to function ${term.name}", term.srcPos)
            else if (memoization.tpe.baseType(Symbols.requiredClass("cps.CpsMonadMemoization.Dynamic")) != NoType) then
                  //
                  //Expr.summon[CpsMonadMemoization.DynamicAp[F,et]] match
                  //                      case Some(mm) =>
                  //                        //TODO: recheck
                  //                        cpsRhs.monadFlatMap( t => '{ ${mm}.apply(${t.asExprOf[et]}) }.asTerm, rhsTpe )
                  //
                  val et = cpsRhs.originType.widen
                  val dynamicApType = AppliedType(Symbols.requiredClassRef("cps.CpsMonadMemoization.DynamicAp"),
                                                  List(summon[CpsTopLevelContext].monadType,et))
                  CpsTransformHelper.findImplicitInstance(dynamicApType, term.span) match
                    case Some(mm) =>
                      cpsRhs.unpure match
                        case Some(unpureRhs) =>
                          CpsTree.impure(term, owner,  Apply(Select(mm,"apply".toTermName),List(unpureRhs)), AsyncKind.Sync)
                        case None =>
                          cpsRhs.asyncKind match
                            case AsyncKind.Sync =>
                              throw CpsTransformException(s"Impossible (Sync asyncKind without unpure) for ${cpsRhs}", term.srcPos)
                            case AsyncKind.Async(internalKind) =>
                              val toMemoizeSym = Symbols.newSymbol(owner, "toMemoize".toTermName, Flags.EmptyFlags, term.tpt.tpe.widen, coord = term.symbol.coord)
                              val toMemoizeValDef = ValDef(toMemoizeSym, EmptyTree).withSpan(term.span)
                              val toMemoizeRef = ref(toMemoizeSym).withSpan(term.span)
                              FlatMapCpsTree(term,owner,cpsRhs,FlatMapCpsTreeArgument(
                                Some(toMemoizeValDef),
                                CpsTree.impure(term, owner,
                                  Apply(Select(mm,"apply".toTermName),List(toMemoizeRef)),
                                  AsyncKind.Sync
                                )
                              ))
                            case AsyncKind.AsyncLambda(bodyKind) =>
                              throw CpsTransformException(s"Can't apply dynamic memoization to async lambda ${term.name}", term.srcPos)
                    case None =>
                      throw CpsTransformException(s"Can't find implicit instance of ${dynamicApType.show}", term.srcPos)
            else
                  throw CpsTransformException(s"Unknown memoization type ${memoization.tpe}", term.srcPos)
      }

}