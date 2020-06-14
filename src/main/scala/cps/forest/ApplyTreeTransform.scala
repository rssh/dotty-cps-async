package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait ApplyTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>
  
  import qctx.tasty.{_, given _}


  /**
   * case Apply(fun,args) 
   *  transform application
   *
   * tails -- list of prepared sequence of curried arguments.
   **/
  def runApply(applyTerm: Term, 
              fun: Term, 
              args: List[Term],
              tails: List[Seq[ApplyArgRecord]]): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       cpsCtx.log(s"runApply, appyTerm=${safeShow(applyTerm)}")
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
                        if (tails.isEmpty)
                          runAwait(applyTerm, awaitArg)
                        else
                          throw MacroError("call of await with multiple parameter lists", posExpr(applyTerm))
                     else
                        // not my await [?]
                        if (cpsCtx.flags.debugLevel >= 10)
                           cpsCtx.log("Not-my-await") 
                        // ??  TODO: apply conversion if exists or touch unchanged
                        handleFunTypeAwaitApply(applyTerm,fun,args,obj,targs, tails)
                   else
                     handleFunTypeApply(applyTerm,fun,args,obj,targs, tails)
            case Select(obj1, name) if (name=="await") =>
                   if ( obj.symbol == awaitSymbol ) 
                     if (targs.head.tpe =:= monadTypeTree.tpe) 
                        if (tails.isEmpty)
                          runAwait(applyTerm, args.head)
                        else
                          throw MacroError("Call of await with multiple param lists",posExpr(applyTerm))
                     else
                        handleFunTypeAwaitApply(applyTerm,fun,args,obj,targs,tails)
                   else
                     handleFunTypeApply(applyTerm, fun, args, obj, targs, tails)
            case _ => handleFunTypeApply(applyTerm, fun, args, obj, targs, tails)
          }
       case Select(obj,method) =>
            handleFunSelect(applyTerm, fun, args, obj, method, tails)
       case Ident(name) =>
            handleFunIdent(applyTerm, fun, args, name, tails)
       case Apply(fun1, args1) => 
            handleFunApply(applyTerm, fun, args, fun1, args1, tails)
       case _ =>
            handleFun(applyTerm, fun, args, tails)

  def handleFunTypeAwaitApply(applyTerm: Term, 
                              awaitFun:Term, 
                              args: List[Term], 
                              obj:Term, 
                              targs:List[TypeTree],
                              tails:List[Seq[ApplyArgRecord]]): CpsTree =
      if (cpsCtx.flags.debugLevel >= 10)
        cpsCtx.log( "runApply:handleFunTypeAwaitApply")
      if (targs.head.tpe =:= monadTypeTree.tpe) 
         if (cpsCtx.flags.debugLevel >= 10)
            cpsCtx.log( "our monad")
         if (tails.isEmpty)
            runAwait(applyTerm, args.head)
         else
            throw MacroError("Unexpected multiple parameters lists for await",posExpr(applyTerm))
      else
         cpsCtx.log(s"targs.head.tpe    = ${targs.head.tpe.show}")
         cpsCtx.log(s"monadTypeTree.tpe = ${monadTypeTree.tpe.show}")
         throw MacroError(s"Interpolation of await between ${targs.head.tpe} and ${monadTypeTree.tpe} is not supported yet", cpsCtx.patternCode )
         

 
  /**
   *applyTerm = Apply(fun, args)
   *fun = TypeApply(obj,targs)
   **/      
  def handleFunTypeApply(applyTerm: Term, 
                         fun:Term, 
                         args: List[Term], 
                         obj:Term, 
                         targs:List[TypeTree],
                         tails:List[Seq[ApplyArgRecord]]): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       cpsCtx.log( "runApply:handleFunTypeApply")
       cpsCtx.log(s"obj=${obj}")
       cpsCtx.log(s"obj.symbol=${obj.symbol}")
       cpsCtx.log(s"obj.symbol.paramSymss=${obj.symbol.paramSymss}")
       cpsCtx.log(s"fun.symbol=${fun.symbol}")
       cpsCtx.log(s"targs=${targs}")
     obj match {
        case Select(obj1,method) =>
          val cpsObj1 = runRoot(obj1)
          if (cpsObj1.isAsync) 
              val cpsObj = cpsObj1.monadMap(x => TypeApply(Select(x,obj.symbol),targs), fun.tpe)
              handleArgs1(applyTerm, fun, cpsObj, args, tails)
          else 
              handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)
        case Ident(name) =>
          handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)
        case _ =>
          val cpsObj = runRoot(obj)  
          handleArgs1(applyTerm, fun, cpsObj, args, tails)
     }


  def handleFunSelect(applyTerm:Term, 
                      fun:Term, 
                      args:List[Term], 
                      obj:Term, 
                      methodName: String, 
                      tails: List[Seq[ApplyArgRecord]]): CpsTree = 
     if (cpsCtx.flags.debugLevel >= 10)
       cpsCtx.log( "runApply:handleFunSelect")
       cpsCtx.log(s"obj=${obj}")
       cpsCtx.log(s"obj.symbol=${obj.symbol}")
       cpsCtx.log(s"methodName=${methodName}")
     val cpsObj = runRoot(obj)
     if (cpsObj.isAsync) 
        handleArgs1(applyTerm, fun, cpsObj.monadMap(x => Select(x,fun.symbol), fun.tpe), args, tails)
     else
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)
   

  def handleFunIdent(applyTerm: Term, fun:Term, args:List[Term], name: String, tails: List[Seq[ApplyArgRecord]]):CpsTree =
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)

  def handleFunApply(applyTerm: Term, fun:Term, args: List[Term], 
                                      fun1: Term, args1: List[Term],
                                      tails: List[Seq[ApplyArgRecord]]):CpsTree =
        val paramsDescriptor = MethodParamsDescriptor(fun)
        val argsRecords = O.buildApplyArgsRecords(fun, paramsDescriptor, args, cpsCtx)
        runApply(applyTerm, fun1, args1, argsRecords::tails) 
                 

  def handleFun(applyTerm: Term, fun:Term, args:List[Term], tails: List[Seq[ApplyArgRecord]]):CpsTree =
       val cpsFun = runRoot(fun)
       handleArgs1(applyTerm, fun, cpsFun, args, tails)



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
  def handleArgs1(applyTerm: Term, 
                  fun: Term, cpsFun: CpsTree, 
                  args: List[Term], 
                  tails: List[Seq[ApplyArgRecord]]): CpsTree =  {
        if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s"handleArgs1, fun=${safeShow(fun)}")
            cpsCtx.log(s" fun.symbol=${fun.symbol}")
            cpsCtx.log(s" fun.tpe=${fun.tpe}")
            cpsCtx.log(s" args=${args}")
            cpsCtx.log(s" tails=${tails}")

        def isExistsAsyncArg(argRecords:Seq[ApplyArgRecord]): Boolean =
             argRecords.exists(_.isAsync)

        def isExistsShiftedLambda(argRecords:Seq[ApplyArgRecord]): Boolean =
             argRecords.exists(_.hasShiftedLambda)
            
               
        val paramsDescriptor = MethodParamsDescriptor(fun)
        
        val applyRecords = O.buildApplyArgsRecords(fun, paramsDescriptor, args, cpsCtx)
        val existsAsyncArg = isExistsAsyncArg(applyRecords) || tails.exists(isExistsAsyncArg)
        val existsShiftedLambda = isExistsShiftedLambda(applyRecords) || tails.exists(isExistsShiftedLambda)
        if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s"existsShiftedLambda=${existsShiftedLambda}")
            cpsCtx.log(s"tails.existsShiftedLambda=${tails.exists(isExistsShiftedLambda)}")
            if (tails.exists(isExistsShiftedLambda)) {
              val shiftedRecord = tails.head.find(_.hasShiftedLambda).get
              shiftedRecord match {
                case ApplyArgLambdaRecord(block,index,cpsBody, shifted) =>
                     cpsCtx.log(s"block=${block.seal.show}")
                     cpsCtx.log(s"let's recalculate runRoot(block)")
                     val cpsBlock = runRoot(block)
                     cpsCtx.log(s"cpsBlock = $cpsBlock")
                case _ =>
                     cpsCtx.log("not lambda record")
              }
            }

        if (!existsAsyncArg && !existsShiftedLambda) {
           if (!cpsFun.isAsync)
              CpsTree.pure(applyTerm)
           else 
              val tailArgss = tails.map(_.map(_.term).toList)
              cpsFun.monadMap(x => x.appliedToArgss(args::tailArgss), applyTerm.tpe)
        } else {
           var runFold = true
           val lastCpsTree: CpsTree = if (!existsAsyncArg && cpsFun.isSync) {
                                    runFold = false
                                    if (!existsShiftedLambda) {
                                       CpsTree.pure(applyTerm)
                                    } else {
                                       buildApply(cpsFun, fun, applyRecords, applyTerm.tpe, existsShiftedLambda, tails)
                                    }
                                 } else {
                                    buildApply(cpsFun, fun, applyRecords, applyTerm.tpe, existsShiftedLambda, tails)
                                 }
           if cpsCtx.flags.debugLevel >= 15 then
               cpsCtx.log(s"handleArgs: runFold=$runFold")
           if (runFold) 
              (applyRecords::tails).foldRight(lastCpsTree){(pa,sa) =>
                 pa.foldRight(sa){ (p,s) =>
                   if (p.useIdent)
                      p.append(s)
                   else
                      s
                 }
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

  object O {  // fix arround https://github.com/lampepfl/dotty/issues/9074

  def buildApplyArgsRecords(fun: qctx.tasty.Term, paramsDescriptor: MethodParamsDescriptor, args: List[qctx.tasty.Term], cpsCtx:TransformationContext[F,?]): List[ApplyArgRecord] = {
     buildApplyArgsRecordsAcc(fun, paramsDescriptor, args, cpsCtx, BuildApplyArgsAcc()).records.toList
  }

  def buildApplyArgsRecordsAcc(fun: Term, paramsDescriptor: MethodParamsDescriptor, args: List[Term], cpsCtx:TransformationContext[F,?], acc: BuildApplyArgsAcc): BuildApplyArgsAcc = {
     args.foldLeft(acc){ (s,e) =>
       buildApplyArgRecord(fun, paramsDescriptor, e, cpsCtx, s)
    }

  }

  def buildApplyArgRecord(fun:Term, paramsDescriptor: MethodParamsDescriptor, t: Term, cpsCtx: TransformationContext[F,?], acc:BuildApplyArgsAcc): BuildApplyArgsAcc = {
       import scala.internal.quoted.showName
       import scala.quoted.QuoteContext
       import scala.quoted.Expr

       if cpsCtx.flags.debugLevel >= 15 then
          cpsCtx.log(s"buildApplyArgRecord: pos=${acc.posIndex}, t=${safeShow(t)} ")
       t match {
         case tr@Typed(r@Repeated(rargs, tpt),tpt1) => 
            val accRepeated = O.buildApplyArgsRecordsAcc(fun, paramsDescriptor, rargs, cpsCtx.nestSame("r"), 
                               acc.copy(inRepeat=true,records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList)
            acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
         case r@Repeated(rargs, tpt) => 
            val accRepeated = O.buildApplyArgsRecordsAcc(fun, paramsDescriptor, rargs, cpsCtx.nestSame("r"), 
                               acc.copy(inRepeat=true, records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList)
            acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
         case lambda@Lambda(params, body) => 
            // mb, this will not work, for expressions, which return block.
            //  look's like somewhere in future, better add 'shifted' case to CpsExpr
            val cpsBody = runRoot(body)
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
                val nested = buildApplyArgRecord(fun,paramsDescriptor,arg,cpsCtx,namedAcc).records.head
                if (realIndex == acc.paramIndex && !acc.wasNamed)
                  acc.advance(nested)
                else
                  acc.advanceNamed(nested,realIndex)
              case None =>  
                 throw MacroError(s"Can't find parameter with name $name", posExpr(t))
         case Block(Nil,last) => 
            buildApplyArgRecord(fun,paramsDescriptor,last,cpsCtx,acc)
         case inlined@Inlined(call,bindings,body) => 
            val nested = buildApplyArgRecord(fun,paramsDescriptor, body,cpsCtx, 
                                                acc.copy(records=IndexedSeq.empty)).records.head
            if (bindings.isEmpty)
               acc.advance(nested)
            else
               acc.advance(ApplyArgInlinedRecord(inlined, nested))
         case _ =>
            if cpsCtx.flags.debugLevel >= 15 then
               cpsCtx.log(s"paramType=${paramsDescriptor.paramType(acc.paramIndex)}")
               cpsCtx.log(s"byName=${paramsDescriptor.isByName(acc.paramIndex)}")
            val termCpsTree = runRoot(t)
            if (paramsDescriptor.isByName(acc.paramIndex))
               acc.advance(ApplyArgByNameRecord(t,acc.posIndex,termCpsTree,termCpsTree.isAsync))
            else
              if (!termCpsTree.isAsync && termIsNoOrderDepended(t)) 
                acc.advance(ApplyArgNoPrecalcTermRecord(t,acc.posIndex))
              else
                val argName: String = "a" + acc.posIndex // TODO: get name from params
                val symbol = Symbol.newVal(summon[Context].owner,argName,t.tpe.widen,Flags.EmptyFlags,Symbol.noSymbol)
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

  def haveAsyncLambdaInArgs(args:List[Term]):Boolean = 
     args.exists{ x =>
       x match
          case Lambda(largs,body) => TransformUtil.containsAwait(body)
          case Repeated(rargs,tpt) => haveAsyncLambdaInArgs(rargs)
          case _ => false
     }

  def findAsyncShift[E:quoted.Type](e:Expr[E]):Option[Expr[AsyncShift[E]]] =
    Expr.summon[AsyncShift[E]]

  def findObjectAsyncShift[E:quoted.Type](e:Expr[E]):Option[Expr[ObjectAsyncShift[E]]]=
    Expr.summon[ObjectAsyncShift[E]]

  def findObjectAsyncShiftTerm(e:Term):ImplicitSearchResult =
    val tpe = e.tpe.widen
    val objAsyncShift = TypeIdent(Symbol.classSymbol("cps.ObjectAsyncShift")).tpe
    val tpTree = AppliedType(objAsyncShift,List(tpe))
    //val tpTree = AppliedType('[ObjectAsyncShift].unseal.tpe,List(tpe)).simplified
    if cpsCtx.flags.debugLevel >= 15 then
      cpsCtx.log(s"searchImplicits: tpTree=$tpTree")
      cpsCtx.log(s"tpe=$tpe")
      cpsCtx.log(s"'[ObjectAsyncShift].unseal.tpe=${'[ObjectAsyncShift].unseal.tpe}")
    searchImplicit(tpTree) 
      

  def findAsyncShifted[E:quoted.Type](e:Expr[E]): ImplicitSearchResult =
    val tpTree = '[AsyncShifted[E,F]].unseal
    searchImplicit(tpTree.tpe) 

  def findAsyncShiftedTerm(e:Term): ImplicitSearchResult = 
     val asyncShifted = '[AsyncShifted].unseal.tpe
     val monad = fType.unseal.tpe
     val targetType = AppliedType(asyncShifted,List(e.tpe,monad))
     searchImplicit(targetType) 


  def shiftedApply(term: Term, args: List[Term], shiftedIndexes:Set[Int]): Term =

    def shiftQual(x:Term):Term = 
       x.seal match 
         case '{ $e: $et} =>
            findAsyncShift(e) match
              case Some(shifted) => shifted.unseal
              case None => 
                   // check, if this is our monad map/flatMap - then we 
                   if cpsCtx.flags.debugLevel > 10 then
                      cpsCtx.log(s"Can't find async shift, x=$x")
                   // TODO: provide some other alternatives ?
                   throw MacroError(s"Can't find AsyncShift for ${et.show} (e=${e.show})",x.seal)
         case _ =>
            throw MacroError(s"Can't find AsyncShift for ${x} (x is not typed)",x.seal)

    def shiftSelect(x:Select):Select = 
          Select.unique(shiftQual(x.qualifier),x.name)
       
    def shiftSelectTypeApply(x:Select, targs:List[TypeTree]): Term = 
       val qual = x.qualifier
       val monad = cpsCtx.monad.unseal
       qual.seal match 
         case '{ $e: $et} =>
          if (cpsCtx.flags.debugLevel >= 15)
             cpsCtx.log(s"e=$e, et=${et.unseal}")
          findObjectAsyncShiftTerm(qual) match
            case success1: ImplicitSearchSuccess =>
              val objectShift = success1.tree
               


              //case Some(objectShift) =>
              //TODO: will work only for unique.
              //   change dotty API to find case, where name is not unique
              //    in qualifier
              if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"found objectShift, ${success1.tree}")
                  cpsCtx.log(s"objectShift.tpe = ${objectShift.tpe}")

              val shiftedExpr = Apply(
                                  TypeApply(Select.unique(objectShift, "apply"),fType.unseal::Nil),
                                  List(qual,monad)
                                )

              val newSelect = Select.unique(shiftedExpr, x.name)
              //val newSelect = Select.unique(TypeApply(object.appliedTo(qual),fseal), x.name)
              if targs.isEmpty
                newSelect
              else
                TypeApply(newSelect, targs)
              //success1.tree
            case  failure1: ImplicitSearchFailure =>
              //case  None =>
              findAsyncShift(e) match
                case Some(shift) => 
                  val newSelect = Select.unique(shift.unseal, x.name)
                  TypeApply(newSelect, fType.unseal::targs).appliedTo(qual,monad)
                case None =>
                  throw MacroError(s"Can't find AsyncShift or ObjectAsyncShift for e=${e.show}, qual=${qual} type=${et.show} expt=${failure1.explanation} ",posExpr(x))
         case _ =>
           throw MacroError(s"Can't find AsyncShift or conversion to AsyncShifted for ${x.tpe.seal}, term can't be typed", posExpr(x))


    def shiftCaller(term:Term): Term = 
       if (cpsCtx.flags.debugLevel >= 15)
           cpsCtx.log(s"shiftCaller, t=$term")
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
                    //val newSelect = shiftSelect(s)
                    //TypeApply(newSelect, fType.unseal::targs).appliedTo(qual,monad)
                    shiftSelectTypeApply(s, targs)
          case s@Select(qual,name) =>
                  //TypeApply(shiftSelect(s), fType.unseal::Nil).appliedTo(qual, monad)
                  shiftSelectTypeApply(s, Nil)
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
                  val errorExpr = posExpr(term)
                  throw MacroError(s"Can't shift caller ${term}",errorExpr)

    Apply(shiftCaller(term),args)

  end shiftedApply

  def buildApply(cpsFun: CpsTree, fun: Term, 
                 argRecords: Seq[ApplyArgRecord], 
                 applyTpe: Type, 
                 withShiftedLambda: Boolean, 
                 tails: List[Seq[ApplyArgRecord]]
                 ): CpsTree = 
        if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"buildApply: fun=${safeShow(fun)}")
            cpsCtx.log(s"cpsFun.isSync=${cpsFun.isSync}, withShiftedLambda=${withShiftedLambda}")
            cpsCtx.log(s"tails.length=${tails.length}")
        if (withShiftedLambda)
          if (cpsFun.isSync) 
            CpsTree.impure(buildShiftedApply(fun, argRecords, tails), applyTpe)
          else 
            cpsFun.monadFlatMap(x => buildShiftedApply(x, argRecords, tails),applyTpe) 
        else
          val args = argRecords.map(_.identArg).toList
          val tailArgss = tails.map(_.map(_.identArg).toList)
          val argss = args::tailArgss
          if (cpsFun.isSync) 
            cpsFun.applyTerm(x => x.appliedToArgss(argss), applyTpe)
          else 
            cpsFun.monadMap(x => x.appliedToArgss(argss), applyTpe)


  def buildShiftedApply(fun: Term, 
                        argRecords:Seq[ApplyArgRecord], 
                        tails:List[Seq[ApplyArgRecord]]): Term =
      val shiftedIndexes = argRecords.zipWithIndex.filter(_._1.hasShiftedLambda).map(_._2)
      val args = argRecords.map(_.shift().identArg).toList
      if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"buildShiftedApply::argRecords=${argRecords}")
          cpsCtx.log(s"buildShiftedApply::args=${args}")
      val shiftedApplyHead = shiftedApply(fun, args, shiftedIndexes.toSet)
      shiftedApplyHead.appliedToArgss(tails.map(_.map(_.shift().identArg).toList))

     
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
                         args.asInstanceOf[List[qctx.tasty.Term]],
                         Nil
                        ).toResult[T]

     }
     (new Bridge(cpsCtx1)).bridge()
  }


