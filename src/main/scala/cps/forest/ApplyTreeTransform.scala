package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait ApplyTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>
  
  import qctx.tasty.{_, given _}


  // case Apply(fun,args) 
  def runApply(applyTerm: Term, 
              fun: Term, 
              args: List[Term]): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       println(s"runApply, appyTerm=${applyTerm}")
     val monad = cpsCtx.monad
     // try to omit things, which should be eta-expanded,
     fun match 
       case TypeApply(obj,targs) =>
          // check - maybe this is await
          obj match {
            case Ident(name) if (name=="await") =>
                   if ( obj.symbol == awaitSymbol ) 
                     if (targs.head.tpe =:= monadTypeTree.tpe) 
                         // TODO: check, that args have one element
                        val awaitArg = args.head
                        runAwait(applyTerm, awaitArg)
                     else
                        // not my await [?]
                        if (cpsCtx.flags.debugLevel >= 10)
                           println("Not-my-await") 
                        // ??  TODO: apply conversion if exists or touch unchanged
                        handleFunTypeAwaitApply(applyTerm,fun,args,obj,targs)
                   else
                     handleFunTypeApply(applyTerm,fun,args,obj,targs)
            case Select(obj1, name) if (name=="await") =>
                   if ( obj.symbol == awaitSymbol ) 
                     if (targs.head.tpe =:= monadTypeTree.tpe) 
                        runAwait(applyTerm, args.head)
                     else
                        handleFunTypeAwaitApply(applyTerm,fun,args,obj,targs)
                   else
                     handleFunTypeApply(applyTerm,fun,args,obj,targs)
            case _ => handleFunTypeApply(applyTerm, fun, args, obj, targs)
          }
       case Select(obj,method) =>
            handleFunSelect(applyTerm, fun, args, obj, method)
       case Ident(name) =>
            handleFunIdent(applyTerm, fun, args, name)
       case _ =>
            handleFun(applyTerm, fun, args)

  def handleFunTypeAwaitApply(applyTerm: Term, 
                              awaitFun:Term, 
                              args: List[Term], 
                              obj:Term, targs:List[TypeTree]): CpsTree =
      if (cpsCtx.flags.debugLevel >= 10)
        println( "runApply:handleFunTypeAwaitApply")
      if (targs.head.tpe =:= monadTypeTree.tpe) 
         if (cpsCtx.flags.debugLevel >= 10)
            println( "our monad")
         runAwait(applyTerm, args.head)
      else
         println(s"targs.head.tpe    = ${targs.head.tpe.show}")
         println(s"monadTypeTree.tpe = ${monadTypeTree.tpe.show}")
         throw MacroError(s"Interpolation of await between ${targs.head.tpe} and ${monadTypeTree.tpe} is not supported yet", cpsCtx.patternCode )
         

 
  /**
   * applyTerm = Apply(fun, args)
   * fun = TypeApply(obj,targs)
   **/      
  def handleFunTypeApply(applyTerm: Term, 
                         fun:Term, 
                         args: List[Term], 
                         obj:Term, 
                         targs:List[TypeTree]): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       println( "runApply:handleFunTypeApply")
       println(s"obj=${obj}")
       println(s"obj.symbol=${obj.symbol}")
       println(s"obj.symbol.paramSymss=${obj.symbol.paramSymss}")
       println(s"fun.symbol=${fun.symbol}")
       println(s"targs=${targs}")
     obj match {
        case Select(obj1,method) =>
          val cpsObj1 = runRoot(obj1)
          if (cpsObj1.isAsync) 
              val cpsObj = cpsObj1.monadMap(x => TypeApply(Select(x,obj.symbol),targs), fun.tpe)
              handleArgs1(applyTerm, fun, cpsObj, args)
          else 
              handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)
        case Ident(name) =>
          handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)
        case _ =>
          val cpsObj = runRoot(obj)  
          handleArgs1(applyTerm, fun, cpsObj, args)
     }


  def handleFunSelect(applyTerm:Term, fun:Term, args:List[Term], obj:Term, methodName: String): CpsTree = 
     if (cpsCtx.flags.debugLevel >= 10)
       println( "runApply:handleFunSelect")
       println(s"   obj=${obj}")
       println(s"   obj.symbol=${obj.symbol}")
       println(s"   methodName=${methodName}")
     val cpsObj = runRoot(obj)
     if (cpsObj.isAsync) 
        handleArgs1(applyTerm, fun, cpsObj.monadMap(x => Select(x,fun.symbol), fun.tpe), args)
     else
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)
   

  def handleFunIdent(applyTerm: Term, fun:Term, args:List[Term], name: String):CpsTree =
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args)

  def handleFun(applyTerm: Term, fun:Term, args:List[Term]):CpsTree =
     val cpsFun = runRoot(fun)
     handleArgs1(applyTerm, fun, cpsFun, args)



  def typeOrBoundsToType(x: TypeOrBounds, isHight: Boolean = true): Type =
    x match 
      case y: Type => y
      case TypeBounds(low,hight) => if (isHight) hight else low
      case NoPrefix => if (isHight) defn.AnyType else defn.NothingType

  def shiftedLambdaTypeTree(tpt: TypeTree): TypeTree =
    Inferred(shiftedLambdaType(tpt.tpe))

  def shiftedLambdaType(tpe: Type): Type =
    tpe.widen match {
      case MethodType(paramNames, paramTypes, resType) =>
               // currently no support for path-dependend lambdas.
               MethodType(paramNames)( mt => paramTypes, 
                                       mt => AppliedType(fType.unseal.tpe, List(resType)))
      case PolyType(paramNames,paramBounds,resType) =>
               PolyType(paramNames)(pt => paramBounds,
                                    pt => AppliedType(fType.unseal.tpe, List(resType)))
      case _ => throw MacroError("Not supported type for shifting: ${tpe}",cpsCtx.patternCode)
    }
   
  /**
   * How to handle arguments?
   *  We want keep evaluation order from left to right, so, imagine we have 
   *  function f(a1,a2,a3) and a2 is async, a1, a3 - sync.
   *  we will transform this to
   *  ```
   *  { val arg1 = a1; 
   *    transform(a2).flatMap( x => 
   *       { val arg2 = x; 
   *         val arg3 = a3;  
   *         f(arg1, arg2, arg3) 
   *  }    }
   *  ```
   *
   * more generally we at first generate block:
   *   { arg1 = a1; .....  argN = aN; f(arg1,...argN) }
   * and then transform one, knowing that all arguments to f are sync
   *  (of course, if all arguments are sync, we just call f(arg1,... arg2) without all this machinery)
   *
   * And we don't want to generate tree and then resu 
   *
  **/

  def handleArgs1(applyTerm: Term, fun: Term, cpsFun: CpsTree, 
                      args: List[Term]): CpsTree =  {
        if cpsCtx.flags.debugLevel >= 15 then
            println(s"handleArgs1, fun=${fun}")
            println(s" fun.symbol=${fun.symbol}")
            println(s" fun.tpe=${fun.tpe}")
            println(s" fun.tpe.simplified=${fun.tpe.simplified}")
            println(s" fun.tpe.widen.dealias=${fun.tpe.widen.dealias}")
            fun.tpe.widen.dealias match 
              case mt@MethodType(paramNames, paramTypes, resultType) =>
                 println(s" methodType: paramTypes=${paramTypes}")
              case pt@PolyType(names, paramBounds, resultBound) =>
                 println(s" polyType: paramBounds=${paramBounds}")
              case _ =>
                 println(s" not a method.")
            
               

        
        val paramSyms = fun.symbol.paramSymss.head.toIndexedSeq
        if (paramSyms.isEmpty && !args.isEmpty)
            warning(s"can't find paramDefs for args, fun=$fun",fun.symbol.pos)
        val applyRecords = buildApplyArgsRecords(fun, paramSyms, args, cpsCtx)
        val existsAsyncArgs = applyRecords.exists(_.isAsync)
        val shiftedLambdaIndexes = applyRecords.filter(_.hasShiftedLambda).map(_.index).toSet
        if (!existsAsyncArgs && shiftedLambdaIndexes.isEmpty) {
           if (!cpsFun.isAsync)
              CpsTree.pure(applyTerm)
           else  
              cpsFun.monadMap(x => Apply(x,args), applyTerm.tpe)
        } else {
           val newArgs = applyRecords.map{ a =>
                 if (!shiftedLambdaIndexes.isEmpty) 
                    a.shift()
                 else 
                    a
                }.map(_.identArg)
           val allArgsAreSync = applyRecords.forall(! _.isAsync )
           var runFold = true
           val lastCpsTree: CpsTree = if (allArgsAreSync && cpsFun.isSync) {
                                    runFold = false
                                    if (shiftedLambdaIndexes.isEmpty) {
                                       CpsTree.pure(applyTerm)
                                    } else {
                                       buildApply(cpsFun, fun, newArgs, applyTerm.tpe, shiftedLambdaIndexes)
                                    }
                                 } else {
                                    buildApply(cpsFun, fun, newArgs, applyTerm.tpe, shiftedLambdaIndexes)
                                 }
           if (runFold) 
              if (cpsCtx.flags.debugLevel >= 15)
                 println(s"lastCpsTree = ${lastCpsTree}")
              applyRecords.foldRight(lastCpsTree){ (p,s) =>
                 if (p.useIdent)
                   p.append(s)
                 else
                   s
              }
           else
              lastCpsTree
        }
  }


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


  def buildApplyArgsRecords(fun: Term, paramSyms: IndexedSeq[Symbol], args: List[Term], cpsCtx:TransformationContext[F,?]): List[ApplyArgRecord] = {
     if cpsCtx.flags.debugLevel >= 15 then
       println(s"buildApplyArgRecords, paramSyms=${paramSyms}")
       println(s"fun.symbol.tree=${fun.symbol.tree}")
     buildApplyArgsRecordsAcc(fun, paramSyms, args, cpsCtx, BuildApplyArgsAcc()).records.toList
  }

  def buildApplyArgsRecordsAcc(fun: Term, paramSyms: IndexedSeq[Symbol], args: List[Term], cpsCtx:TransformationContext[F,?], acc: BuildApplyArgsAcc): BuildApplyArgsAcc = {
     if cpsCtx.flags.debugLevel >= 15 then
       println(s"buildApplyArgRecordsAcc")
     
     args.foldLeft(acc){ (s,e) =>
       buildApplyArgRecord(fun, paramSyms, e, cpsCtx, s)
    }

  }

  def buildApplyArgRecord(fun:Term, paramSyms: IndexedSeq[Symbol], t: Term, cpsCtx: TransformationContext[F,?], acc:BuildApplyArgsAcc): BuildApplyArgsAcc = {
       import scala.internal.quoted.showName
       import scala.quoted.QuoteContext
       import scala.quoted.Expr
       if cpsCtx.flags.debugLevel >= 15 then
          println(s"buildApplyArgRecord: pos=${acc.posIndex} t=$t, t.tpe=${t.tpe}")
          // TODO: this will not work with NamedArgs
          //println(s"paramSyms(i)=${paramSyms(i)}")
          //println(s"paramSyms(i).tree=${paramSyms(i).tree}")
       t match {
         case tr@Typed(r@Repeated(rargs, tpt),tpt1) => 
            val accRepeated = buildApplyArgsRecordsAcc(fun, paramSyms, rargs, cpsCtx.nestSame("r"), 
                               acc.copy(inRepeat=true,records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList)
            acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
         case r@Repeated(rargs, tpt) => 
            val accRepeated = buildApplyArgsRecordsAcc(fun, paramSyms, rargs, cpsCtx.nestSame("r"), 
                               acc.copy(inRepeat=true, records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList)
            acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
         case lambda@Lambda(params, body) => 
            // mb, this will not work, for expressions, which return block.
            //  look's like somewhere in future, better add 'shifted' case to CpsExpr
            val cpsBody = runRoot(body)
            val nextRecord = ApplyArgLambdaRecord(lambda,acc.posIndex,cpsBody, false)
            acc.advance(nextRecord)
         case namedArg@NamedArg(name, arg) =>
             // TODO: change i ?
            val realIndex = paramSyms.indexWhere(_.name == name)
            if (realIndex >= 0) then
               val namedAcc = acc.copy(inNamed=true,paramIndex = realIndex, records=IndexedSeq.empty)
               val nested = buildApplyArgRecord(fun,paramSyms,arg,cpsCtx,namedAcc).records.head
               if (realIndex == acc.paramIndex && !acc.wasNamed)
                 acc.advance(nested)
               else
                 acc.advanceNamed(nested,realIndex)
            else
               throw MacroError(s"Can't find parameter with name $name", safeSeal(t))
         case Block(Nil,last) => 
            buildApplyArgRecord(fun,paramSyms,last,cpsCtx,acc)
         case inlined@Inlined(call,bindings,body) => 
            val nested = buildApplyArgRecord(fun,paramSyms, body,cpsCtx, acc.copy(records=IndexedSeq.empty)).records.head
            if (bindings.isEmpty)
               acc.advance(nested)
            else
               acc.advance(ApplyArgInlinedRecord(inlined, nested))
         case _ =>
            // TODO: check pass by name before
            val termCpsTree = runRoot(t)
            if (!termCpsTree.isAsync && termIsNoOrderDepended(t)) 
               acc.advance(ApplyArgNoPrecalcTermRecord(t,acc.posIndex))
            else
               val argName: String = "a" + acc.posIndex // TODO: get name from params
               val symbol = Symbol.newVal(summon[Context].owner,argName,t.tpe.widen,Flags.EmptyFlags,Symbol.noSymbol)
               val valDef = symbol.tree match
                 case v@ValDef(_,_,_) => v
                 case _ =>
                    throw MacroError("Impossible internal error, create ValDef but have ${symbol.tree", safeSeal(t))
               val ident = Ref(symbol)
               if (cpsCtx.flags.debugLevel > 15) {
                    println(s"buildApplyArg: Precacl, t=$t, i=${acc.posIndex}")
               }
               acc.advance(ApplyArgPrecalcTermRecord(t,acc.posIndex,termCpsTree,valDef,ident))
       }
  }

  def haveAsyncLambdaInArgs(args:List[Term]):Boolean = 
     args.exists{ x =>
       x match
          case Lambda(largs,body) => TransformUtil.containsAwait(body)
          case Repeated(rargs,tpt) => haveAsyncLambdaInArgs(rargs)
          case _ => false
     }

  def findAsyncShift[E:quoted.Type](e:Expr[E]):Option[Expr[AsyncShift[E]]] =
    Expr.summon[AsyncShift[E]]
      

  def shiftedApply(term: Term, args: List[Term], shiftedIndexes:Set[Int]): Term =

    def shiftQual(x:Term):Term = 
       x.seal match 
         case '{ $e: $et} =>
            findAsyncShift(e) match
              case Some(shifted) => shifted.unseal
              case None => 
                   // check, if this is our monad map/flatMap - then we 
                   if cpsCtx.flags.debugLevel > 10 then
                      println(s"Can't find async shift, x=$x")
                   // TODO: provide some other alternatives ?
                   throw MacroError(s"Can't find AsyncShift for ${et.show} (e=${e.show})",x.seal)
         case _ =>
            throw MacroError(s"Can't find AsyncShift for ${x} (x is not typed)",x.seal)

    //TODO: will work only for unique.
    //   change dotty API to find case, where name is not unique
    //    in qualifier
    def shiftSelect(x:Select):Select = 
          Select.unique(shiftQual(x.qualifier),x.name)
       
    def shiftCaller(term:Term): Term = 
       val monad = cpsCtx.monad.unseal
       term match
          case TypeApply(s@Select(qual,name),targs) =>
                  if (qual.tpe =:= monad.tpe) 
                    if (s.symbol == mapSymbol) 
                      val newSelect = Select.unique(shiftQual(s.qualifier),s.name)
                      TypeApply(newSelect, targs).appliedTo(qual)
                    else if (s.symbol == flatMapSymbol)
                      val newSelect = Select.unique(shiftQual(s.qualifier),s.name)
                      TypeApply(newSelect, targs).appliedTo(qual)
                    else
                      throw new MacroError("Unimplemented shift for CpsMonad", term.seal) 
                  else
                    val newSelect = shiftSelect(s)
                    TypeApply(newSelect, fType.unseal::targs).appliedTo(qual,monad)
          case s@Select(qual,name) =>
                  TypeApply(shiftSelect(s), fType.unseal::Nil).appliedTo(qual, monad)
          case TypeApply(x, targs) => 
                  TypeApply(shiftCaller(x),targs)
          case Apply(x, args) => 
                  // TODO: shift args
                  Apply(shiftCaller(x),args)
          case Lambda(params, body) =>
                  val shiftedSymbols = params.zipWithIndex.filter{ 
                      (p,i) => shiftedIndexes.contains(i)
                  }.map{ (p,i) => p.symbol }.toSet 
                  val nBody = asyncShift(body, shiftedSymbols)
                  ???
          case Block(statements, last) =>
                  Block(statements, shiftCaller(last))
          case _ => 
                  val errorExpr = safeSeal(term)
                  throw MacroError(s"Can't shift caller ${term}",errorExpr)

    Apply(shiftCaller(term),args)

  end shiftedApply

  def buildApply(cpsFun: CpsTree, fun: Term, args: List[Term], applyTpe: Type, shiftedIndexes: Set[Int]): CpsTree = 
        if (!shiftedIndexes.isEmpty)
          if (cpsFun.isSync) 
            CpsTree.impure(shiftedApply(fun, args, shiftedIndexes), applyTpe)
          else
            cpsFun.monadFlatMap(x => shiftedApply(x,args, shiftedIndexes),applyTpe) 
        else
          if (cpsFun.isSync) 
            cpsFun.applyTerm(_.appliedToArgs(args), applyTpe)
          else
            cpsFun.monadMap(x => Apply(x,args), applyTpe)

     
object ApplyTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: QuoteContext)(cpsCtx1: TransformationContext[F,T],
                         applyTerm: qctx1.tasty.Term,
                         fun: qctx1.tasty.Term,
                         args: List[qctx1.tasty.Term]): CpsExpr[F,T] = {
     //val tmpCpsCtx = cpsCtx
     val tmpQctx = qctx1
     val tmpFtype = summon[Type[F]]
     val tmpCTtype = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType = tmpFtype
         implicit val ctType = tmpCTtype

         def bridge(): CpsExpr[F,T] =
            runApply(applyTerm.asInstanceOf[qctx.tasty.Term],
                         fun.asInstanceOf[qctx.tasty.Term],
                         args.asInstanceOf[List[qctx.tasty.Term]]
                        ).toResult[T]

     }
     (new Bridge(cpsCtx1)).bridge()
  }


