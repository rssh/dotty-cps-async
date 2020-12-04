package cps.forest.application

import cps.{TransformationContextMarker=>TCM, _}
import cps.forest._
import cps.misc._

trait ApplyArgBuilderScope[F[_],CT] {

  thisTreeTransform: TreeTransformScope[F,CT] =>

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
        copy(posIndex=posIndex+1, records = records.appended(record))
      else if (inNamed)
        copy(posIndex=posIndex+1, wasNamed = true, records = records.appended(record))
      else
        copy(posIndex=posIndex+1, paramIndex=paramIndex+1, records = records.appended(record))


    def advanceNamed(record: ApplyArgRecord, index: Int): BuildApplyArgsAcc =
      if (wasNamed || paramIndex == 0)
        copy(records = records.appended(record), filledNamed = filledNamed + index)
      else
        copy(wasNamed = true,
            records = records.appended(record), filledNamed = (0 until paramIndex).toSet + index)


  }

  object O {  // fix arround https://github.com/lampepfl/dotty/issues/9074

    def buildApplyArgsRecords(paramsDescriptor: MethodParamsDescriptor, args: List[qctx.reflect.Term], cpsCtx:TransformationContext[F,?]): List[ApplyArgRecord] = {
     buildApplyArgsRecordsAcc(paramsDescriptor, args, cpsCtx, BuildApplyArgsAcc()).records.toList
    }

    def buildApplyArgsRecordsAcc(paramsDescriptor: MethodParamsDescriptor, args: List[Term], cpsCtx:TransformationContext[F,?], acc: BuildApplyArgsAcc): BuildApplyArgsAcc = {
       args.foldLeft(acc){ (s,e) =>
         buildApplyArgRecord(paramsDescriptor, e, cpsCtx, s)
       }

    }

    def buildApplyArgRecord(paramsDescriptor: MethodParamsDescriptor, t: Term, cpsCtx: TransformationContext[F,?], acc:BuildApplyArgsAcc): BuildApplyArgsAcc = {
       import scala.quoted.Quotes
       import scala.quoted.Expr

       if cpsCtx.flags.debugLevel >= 15 then
          cpsCtx.log(s"buildApplyArgRecord: pos=${acc.posIndex}, t=${safeShow(t)} ")
       t match {
         case tr@Typed(r@Repeated(rargs, tpt),tpt1) =>
            val accRepeated = O.buildApplyArgsRecordsAcc(paramsDescriptor,
                               rargs, cpsCtx.nestSame(TCM.Repeated),
                               acc.copy(inRepeat=true,records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList, tpt1)
            acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
         case r@Repeated(rargs, tpt) =>
            val accRepeated = O.buildApplyArgsRecordsAcc(paramsDescriptor,
                               rargs, cpsCtx.nestSame(TCM.Repeated),
                               acc.copy(inRepeat=true, records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList, tpt)
            acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
         case lambda@Lambda(params, body) =>
            // mb, this will not work, for expressions, which return block.
            //  look's like somewhere in future, better add 'shifted' case to CpsExpr
            val cpsBody = runRoot(body, TCM.ApplyArg(acc.posIndex))
            val nextRecord = if (paramsDescriptor.isByName(acc.paramIndex)) {
                               throw MacroError("passing lamda as byName params is not supported yet",posExpr(t))
                             } else {
                               ApplyArgLambdaRecord(lambda,acc.posIndex,cpsBody, false)
                             }
            acc.advance(nextRecord)
         case namedArg@NamedArg(name, arg) =>
            paramsDescriptor.paramIndex(name) match
              case Some(realIndex) =>
                val namedAcc = acc.copy(inNamed=true,paramIndex = realIndex, records=IndexedSeq.empty)
                val nested = buildApplyArgRecord(paramsDescriptor,arg,cpsCtx,namedAcc).records.head
                if (realIndex == acc.paramIndex && !acc.wasNamed)
                  acc.advance(nested)
                else
                  acc.advanceNamed(nested,realIndex)
              case None =>
                 throw MacroError(s"Can't find parameter with name $name", posExpr(t))
         case Block(Nil,last) =>
            buildApplyArgRecord(paramsDescriptor,last,cpsCtx,acc)
         case inlined@Inlined(call,bindings,body) =>
            val nested = buildApplyArgRecord(paramsDescriptor, body,cpsCtx,
                                                acc.copy(records=IndexedSeq.empty)).records.head
            if (bindings.isEmpty)
               acc.advance(nested)
            else
               acc.advance(ApplyArgInlinedRecord(inlined, nested))
         case _ =>
            if cpsCtx.flags.debugLevel >= 15 then
               cpsCtx.log(s"paramType=${paramsDescriptor.paramType(acc.paramIndex)}")
               cpsCtx.log(s"byName=${paramsDescriptor.isByName(acc.paramIndex)}")
            val termCpsTree = runRoot(t, TCM.ApplyArg(acc.posIndex) )
            if cpsCtx.flags.debugLevel >= 15 then
               cpsCtx.log(s"termCpsTree = ${termCpsTree}")
               cpsCtx.log(s"termCpsTree.isAsync = ${termCpsTree.isAsync}")

            if (paramsDescriptor.isByName(acc.paramIndex))
               acc.advance(ApplyArgByNameRecord(t,acc.posIndex,termCpsTree,termCpsTree.isAsync))
            else
              if (!termCpsTree.isAsync && termIsNoOrderDepended(t))
                acc.advance(ApplyArgNoPrecalcTermRecord(t,acc.posIndex))
              else
                val argName: String = "a" + acc.posIndex // TODO: get name from params
                // will be the next dotty upgrade
                //val symbol = Symbol.newVal(Owner.current.symbol,argName,t.tpe.widen,Flags.EmptyFlags,Symbol.noSymbol)
                val symbol = Symbol.newVal(Symbol.spliceOwner,argName,t.tpe.widen,Flags.EmptyFlags,Symbol.noSymbol)
                val valDef = symbol.tree match
                  case v@ValDef(_,_,_) => v
                  case _ =>
                    throw MacroError("Impossible internal error, create ValDef but have ${symbol.tree", posExpr(t))
                val ident = Ref(symbol)
                if (cpsCtx.flags.debugLevel > 15)
                    cpsCtx.log(s"buildApplyArg: Precacl, t=$t, i=${acc.posIndex}")
                acc.advance(ApplyArgPrecalcTermRecord(t,acc.posIndex,termCpsTree,valDef,ident))
       }
    }

  }

}
