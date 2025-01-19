package cps.macros.forest.application

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.forest._
import cps.macros.misc._

trait ApplyArgBuilderScope[F[_], CT, CC <: CpsMonadContext[F]] {

  thisTreeTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  case class BuildApplyArgsAcc(
      posIndex: Int = 0,
      paramIndex: Int = 0,
      wasNamed: Boolean = false,
      inRepeat: Boolean = false,
      inNamed: Boolean = false,
      records: Seq[ApplyArgRecord] = IndexedSeq.empty,
      filledNamed: Set[Int] = Set.empty
  ) {

    def advance(record: ApplyArgRecord): BuildApplyArgsAcc =
      if (inRepeat)
        copy(posIndex = posIndex + 1, records = records.appended(record))
      else if (inNamed)
        copy(posIndex = posIndex + 1, wasNamed = true, records = records.appended(record))
      else
        copy(posIndex = posIndex + 1, paramIndex = paramIndex + 1, records = records.appended(record))

    def advanceNamed(record: ApplyArgRecord, index: Int): BuildApplyArgsAcc =
      if (wasNamed || paramIndex == 0)
        copy(records = records.appended(record), filledNamed = filledNamed + index)
      else
        copy(wasNamed = true, records = records.appended(record), filledNamed = (0 until paramIndex).toSet + index)

  }

  object O { // fix arround https://github.com/lampepfl/dotty/issues/9074

    def buildApplyArgsRecords(paramsDescriptor: MethodParamsDescriptor, args: List[qctx.reflect.Term])(
        owner: Symbol
    ): List[ApplyArgRecord] = {
      buildApplyArgsRecordsAcc(paramsDescriptor, args, owner, BuildApplyArgsAcc()).records.toList
    }

    def buildApplyArgsRecordsAcc(
        paramsDescriptor: MethodParamsDescriptor,
        args: List[Term],
        owner: Symbol,
        acc: BuildApplyArgsAcc
    ): BuildApplyArgsAcc = {
      args.foldLeft(acc) { (s, e) =>
        buildApplyArgRecord(paramsDescriptor, e, s)(owner)
      }

    }

    def buildApplyArgRecord(paramsDescriptor: MethodParamsDescriptor, t: Term, acc: BuildApplyArgsAcc)(
        owner: Symbol
    ): BuildApplyArgsAcc = {
      import scala.quoted.Quotes
      import scala.quoted.Expr

      if cpsCtx.flags.debugLevel >= 15 then cpsCtx.log(s"buildApplyArgRecord: pos=${acc.posIndex}, t=${safeShow(t)} ")
      t match {
        case tr @ Typed(r @ Repeated(rargs, tpt), tpt1) =>
          // TODO: in nested context
          val accRepeated =
            O.buildApplyArgsRecordsAcc(paramsDescriptor, rargs, owner, acc.copy(inRepeat = true, records = IndexedSeq.empty))
          val nextRecord = ApplyArgRepeatRecord(tr, r, acc.posIndex, accRepeated.records.toList, tpt1)
          acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
        case r @ Repeated(rargs, tpt) =>
          // TODO: in nested context
          // repeated without typed. Impossible?..  let's put warning and process.
          // val tr = Typed.copy(r)(r,Inferred(r.tpe.widen))
          // report.warning(s"repeated without type, t=${t.show}", posExpr(t))
          val accRepeated =
            O.buildApplyArgsRecordsAcc(paramsDescriptor, rargs, owner, acc.copy(inRepeat = true, records = IndexedSeq.empty))
          val nextRecord = ApplyArgRepeatRecord(r, r, acc.posIndex, accRepeated.records.toList, tpt)
          acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
        case lambda @ Lambda(params, body) =>
          // mb, this will not work, for expressions, which return block.
          //  look's like somewhere in future, better add 'shifted' case to CpsExpr
          val cpsBody = runRoot(body)(owner)
          val nextRecord = if (paramsDescriptor.isByName(acc.paramIndex)) {
            throw MacroError("passing lamda as byName params is not supported yet", posExpr(t))
          } else {
            ApplyArgLambdaRecord(lambda, acc.posIndex, cpsBody, None, None, isInMonad(body.tpe), owner)
          }
          acc.advance(nextRecord)
        case namedArg @ NamedArg(name, arg) =>
          paramsDescriptor.paramIndex(name) match
            case Some(realIndex) =>
              val namedAcc = acc.copy(inNamed = true, paramIndex = realIndex, records = IndexedSeq.empty)
              val nested = buildApplyArgRecord(paramsDescriptor, arg, namedAcc)(owner).records.head
              if (realIndex == acc.paramIndex && !acc.wasNamed)
                acc.advance(nested)
              else
                acc.advanceNamed(nested, realIndex)
            case None =>
              throw MacroError(s"Can't find parameter with name $name", posExpr(t))
        case Block(Nil, last) =>
          buildApplyArgRecord(paramsDescriptor, last, acc)(owner)
        case inlined @ Inlined(call, bindings, body) =>
          if (bindings.isEmpty)
            val nested = buildApplyArgRecord(paramsDescriptor, body, acc.copy(records = IndexedSeq.empty))(owner).records.head
            acc.advance(nested)
          else
            runInlined(inlined)(owner) match
              case inlined @ InlinedCpsTree(inlineOwner, origin, binding, cpsNested) =>
                val nested = buildCpsTreeApplyArgRecord(paramsDescriptor, body, cpsNested, acc.copy(records = IndexedSeq.empty))(
                  owner
                ).records.head
                acc.advance(ApplyArgInlinedRecord(inlined, nested))
              case nonInlined =>
                buildCpsTreeApplyArgRecord(paramsDescriptor, body, nonInlined, acc)(owner)
        case _ =>
          if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s"paramType=${paramsDescriptor.paramType(acc.paramIndex)}")
            cpsCtx.log(s"byName=${paramsDescriptor.isByName(acc.paramIndex)}")
          val termCpsTree = runRoot(t)(owner)
          buildCpsTreeApplyArgRecord(paramsDescriptor, t, termCpsTree, acc)(owner)

      }
    }

    def buildCpsTreeApplyArgRecord(paramsDescriptor: MethodParamsDescriptor, t: Term, termCpsTree: CpsTree, acc: BuildApplyArgsAcc)(
        owner: Symbol
    ): BuildApplyArgsAcc = {
      if cpsCtx.flags.debugLevel >= 15 then
        cpsCtx.log(s"termCpsTree = ${termCpsTree}")
        cpsCtx.log(s"termCpsTree.isAsync = ${termCpsTree.isAsync}")

      if (paramsDescriptor.isByName(acc.paramIndex)) acc.advance(ApplyArgByNameRecord(t, acc.posIndex, termCpsTree, None, None))
      else if (!termCpsTree.isAsync && termIsNoOrderDepended(t)) then
        if (!termCpsTree.isChanged) then
          acc.advance(ApplyArgNoPrecalcTermRecord(t, acc.posIndex, false, paramsDescriptor.isCpsDirect(acc.paramIndex)))
        else
          acc.advance(
            ApplyArgNoPrecalcTermRecord(
              termCpsTree.syncOrigin.get,
              acc.posIndex,
              true,
              paramsDescriptor.isCpsDirect(acc.paramIndex)
            )
          )
      else if (termCpsTree.isLambda)
        termCpsTree match
          case AsyncLambdaCpsTree(owner, originLambda, params, cpsBody, otpe) =>
            val nextRecord = ApplyArgLambdaRecord(originLambda, acc.posIndex, cpsBody, None, None, isInMonad(otpe), owner)
            acc.advance(nextRecord)
          case BlockCpsTree(blockOowner, prevs, last) =>
            // TODO: create instance of ApplyArgLambdaBlockRecord
            throw MacroError(s"Lambda inside blocks is not supported in arguments yet", posExpr(t))
          case _ =>
            throw MacroError(s"Lambda expected", posExpr(t))
      else
        val argName: String = "a" + acc.posIndex // TODO: get name from params
        val widenType = TransformUtil.veryWiden(t.tpe)
        val symbol = Symbol.newVal(Symbol.spliceOwner, argName, widenType, Flags.EmptyFlags, Symbol.noSymbol)
        val valDef = symbol.tree match
          case v @ ValDef(_, _, _) => v
          case _ =>
            throw MacroError("Impossible internal error, create ValDef but have ${symbol.tree}", posExpr(t))
        val ident = Ref(symbol)
        if (cpsCtx.flags.debugLevel > 15)
          cpsCtx.log(s"buildApplyArg: Precacl, t=$t\n, termCpsTree=${termCpsTree}\n, i=${acc.posIndex}")
        acc.advance(
          ApplyArgPrecalcTermRecord(
            t,
            acc.posIndex,
            termCpsTree.castOtpe(widenType),
            valDef,
            ident,
            paramsDescriptor.isCpsDirect(acc.paramIndex)
          )
        )

    }

  }

}
