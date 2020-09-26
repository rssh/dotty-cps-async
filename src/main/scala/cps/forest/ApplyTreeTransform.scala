package cps.forest

import scala.quoted._

import cps.{TransformationContextMarker=>TCM, _}
import cps.misc._

trait ApplyTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.tasty._


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
     val r = fun match
       case TypeApply(obj,targs) =>
            handleFunTypeApply(applyTerm,fun,args,obj,targs, tails)
       case Select(obj,method) =>
            handleFunSelect(applyTerm, fun, args, obj, method, tails)
       case Ident(name) =>
            handleFunIdent(applyTerm, fun, args, name, tails)
       case Apply(fun1@TypeApply(obj2,targs2), args1) if obj2.symbol == awaitSymbol =>
             // catch await early
             runAwait(applyTerm, args1.head, targs2.head.tpe, args.head)
       case Apply(fun1, args1) =>
            handleFunApply(applyTerm, fun, args, fun1, args1, tails)
       case _ =>
            handleFun(applyTerm, fun, args, tails)
     if (cpsCtx.flags.debugLevel > 15)
       cpsCtx.log(s"runApply result = ${r}")
       cpsCtx.log(s"runApply result transformed = ${r.transformed.show}")
     r

  def sameSelect(funTerm:Term, name:String, targs:List[TypeTree], args:List[Term]):Option[Term] =
      funTerm.symbol.method(name) match
        case None => None
        case m::Nil =>
           val select = Select.unique(funTerm, name)
           if (targs.isEmpty)
              Some(select)
           else
              Some(TypeApply(select,targs))
        case other =>
           Select.overloaded(funTerm,name,targs.map(_.tpe),args) match
              case Apply(x,y) => Some(x)
              case other =>
                throw MacroError("expected that Select.overloaded should match apply", posExprs(other,funTerm))
           
     


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
       if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"obj=${obj}")
       cpsCtx.log(s"obj.symbol=${obj.symbol}")
       cpsCtx.log(s"obj.symbol.paramSymss=${obj.symbol.paramSymss}")
       cpsCtx.log(s"fun.symbol=${fun.symbol}")
       cpsCtx.log(s"targs=${targs}")
     obj match {
        case Select(obj1,method) =>
          val cpsObj1 = runRoot(obj1, TCM.ApplyTypeApplySelect)
          if (cpsObj1.isAsync)
              cpsObj1 match
                 case lt: AsyncLambdaCpsTree =>
                        println(s"cpsObj1=$cpsObj1")
                        println(s"method=$method")
                        println(s"targs=$targs")
                        ???
                 case cls: CallChainSubstCpsTree =>
                          // check - is shifted have such name.
                          val shifted = cls.shifted
                          sameSelect(shifted, method, targs, args) match 
                            case None =>
                               // not-found, use origin
                               val cpsObj = cpsObj1.monadMap(x => TypeApply(Select(x,obj.symbol),targs), fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj, args, tails)
                            case Some(term) =>
                               val r = handleArgs1(applyTerm, term, CpsTree.pure(term, isChanged=true), args, tails, unpure=true)
                               if (cpsCtx.flags.debugLevel >= 15)
                                    cpsCtx.log(s"!!!r=$r")
                                    cpsCtx.log(s"r.isChanged=${r.isChanged}")
                               r
                               /*
                               val cpsObj = cpsObj1.monadMap(x => TypeApply(Select(x,obj.symbol),targs), fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj, args, tails)
                               */
                 case _ =>
                     val cpsObj = cpsObj1.monadMap(x => TypeApply(Select(x,obj.symbol),targs), fun.tpe)
                     handleArgs1(applyTerm, fun, cpsObj, args, tails)
          else if (cpsObj1.isChanged)
              val cpsObj = cpsObj1.applyTerm1(x=>TypeApply(Select(x,obj.symbol),targs), fun.tpe)
              handleArgs1(applyTerm, fun, cpsObj, args, tails)
          else
              handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)
        case Ident(name) =>
          handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)
        case _ =>
          val cpsObj = runRoot(obj, TCM.ApplyTypeApply)
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
       if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"obj=${obj}")
     obj match
       case Inlined(_,_,
              Typed(
                 Lambda(List(xValDef),
                   Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1))),
                 cv)
              ) if (obj3.symbol == awaitSymbol
                   && xValDef.symbol == x.symbol) =>
                  // here we catch await, inserted by implicit conversion.
                  // this code is likey depends from implementation details of a compiler
                  // mb create compiler-level API ?
                  runAwait(applyTerm, args.head, targs3.head.tpe, args1.head)
       case _ =>
         val cpsObj = runRoot(obj, TCM.ApplySelect)
         cpsObj match
            case lt: AsyncLambdaCpsTree =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log("funSelect: AsyncLambdaCpsTree discovered, fun=$fun fun.tpe=${fun.tpe}")
               handleArgs1(applyTerm, fun, cpsObj.select(fun.symbol, fun.tpe), args, tails)
            case cls: CallChainSubstCpsTree =>
               sameSelect(cls.shifted, methodName, List.empty, args) match 
                  case None => val cpsObj1 = cpsObj.monadMap(x => Select(x,fun.symbol), fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails)
                  case Some(term) => val cpsObj1 = CpsTree.pure(term, isChanged = true)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails, unpure=true)
            case _ =>
               if (cpsObj.isAsync)
                   handleArgs1(applyTerm, fun,
                        cpsObj.monadMap(x => Select(x,fun.symbol), fun.tpe), args, tails)
               else
                  handleArgs1(applyTerm, fun, cpsObj.select(fun.symbol, fun.tpe), args, tails)


  def handleFunIdent(applyTerm: Term, fun:Term, args:List[Term], name: String, tails: List[Seq[ApplyArgRecord]]):CpsTree =
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)

  def handleFunApply(applyTerm: Term, fun:Term, args: List[Term],
                                      fun1: Term, args1: List[Term],
                                      tails: List[Seq[ApplyArgRecord]]):CpsTree =
        val paramsDescriptor = MethodParamsDescriptor(fun)
        val argsRecords = O.buildApplyArgsRecords(fun, paramsDescriptor, args, cpsCtx)
        runApply(applyTerm, fun1, args1, argsRecords::tails)


  def handleFun(applyTerm: Term, fun:Term, args:List[Term], tails: List[Seq[ApplyArgRecord]]):CpsTree =
       val cpsFun = runRoot(fun, TCM.ApplyFun)
       handleArgs1(applyTerm, fun, cpsFun, args, tails)



  def typeOrBoundsToType(x: Type, isHight: Boolean = true): Type =
    x match
      case TypeBounds(low,hight) => if (isHight) hight else low
      case NoPrefix => if (isHight) Type.of[Any] else Type.of[Nothing]
      case _ => x

  def shiftedLambdaTypeTree(tpt: TypeTree): TypeTree =
    Inferred(shiftedLambdaType(tpt.tpe))

  def shiftedLambdaType(tpe: Type): Type =
    tpe.widen match {
      case MethodType(paramNames, paramTypes, resType) =>
               // currently no support for path-dependend lambdas.
               MethodType(paramNames)( mt => paramTypes,
                                       mt => fType.unseal.tpe.appliedTo(resType))
      case PolyType(paramNames,paramBounds,resType) =>
               PolyType(paramNames)(pt => paramBounds,
                                    pt => fType.unseal.tpe.appliedTo(resType))
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
   *@param applyTerm = Apply(fun, args)  - origin apply
   *@param fun - function to apply (with type-paerameters)
   *@param args - first argument list
   *@param tails - next argument lists if any
   *@param unpure - if true, that this is call from shifted substitution, which is already return F[_] by design.
   *
  **/
  def handleArgs1(applyTerm: Term,
                  fun: Term, 
                  cpsFun: CpsTree,
                  args: List[Term],
                  tails: List[Seq[ApplyArgRecord]],
                  unpure: Boolean = false
                   ): CpsTree =  {
        if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s"handleArgs1, fun=${safeShow(fun)}")
            cpsCtx.log(s" cpsFun=${cpsFun}")
            cpsCtx.log(s" fun.symbol=${fun.symbol}")
            cpsCtx.log(s" fun.tpe=${fun.tpe}")
            cpsCtx.log(s" args=${args}")
            cpsCtx.log(s" tails=${tails}")
            cpsCtx.log(s" unpure=${unpure}")

        def isExistsAsyncArg(argRecords:Seq[ApplyArgRecord]): Boolean =
             argRecords.exists(_.isAsync)

        def isExistsPrependArg(argRecords:Seq[ApplyArgRecord], existsAsync: Boolean): Boolean =
             argRecords.exists(_.usePrepend(existsAsync))

        def isExistsShiftedLambda(argRecords:Seq[ApplyArgRecord]): Boolean =
             argRecords.exists(_.hasShiftedLambda)


        val paramsDescriptor = MethodParamsDescriptor(fun)

        val applyRecords = O.buildApplyArgsRecords(fun, paramsDescriptor, args, cpsCtx)
        val existsAsyncArg = isExistsAsyncArg(applyRecords) || tails.exists(isExistsAsyncArg)
        val existsPrependArg = isExistsPrependArg(applyRecords, existsAsyncArg) ||
                                                         tails.exists(isExistsPrependArg(_, existsAsyncArg))
        val existsShiftedLambda = isExistsShiftedLambda(applyRecords) || tails.exists(isExistsShiftedLambda)
        if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s" existsShiftedLambda=${existsShiftedLambda}")
            cpsCtx.log(s" tails.existsShiftedLambda=${tails.exists(isExistsShiftedLambda)}")
            cpsCtx.log(s" existsAsyncArg=${existsAsyncArg}")
            cpsCtx.log(s" tails.existsAsyncArg=${tails.exists(isExistsAsyncArg)}")
            cpsCtx.log(s" existsPrependArg=${existsPrependArg}")
            cpsCtx.log(s" tails.existsPrependArg=${tails.exists(isExistsPrependArg(_, existsAsyncArg))}")

        if (!existsAsyncArg && !existsShiftedLambda) {
           val tailArgss = tails.map(_.map(_.term).toList)
           cpsFun match
              case lt: AsyncLambdaCpsTree =>
                      CpsTree.impure(Select.unique(lt.rLambda,"apply").appliedToArgss(args::tailArgss), applyTerm.tpe)
              case _ =>
                      cpsFun.syncOrigin match
                         case Some(fun1) =>
                           if (!cpsFun.isChanged && !unpure) 
                             CpsTree.pure(applyTerm)
                           else if (unpure)
                             CpsTree.impure(fun1.appliedToArgss(args::tailArgss), applyTerm.tpe)
                           else
                             CpsTree.pure(fun1.appliedToArgss(args::tailArgss), true)
                         case _ => 
                            cpsFun.monadMap(x => x.appliedToArgss(args::tailArgss), applyTerm.tpe)
        } else {
           var runFold = true
           val lastCpsTree: CpsTree = if (!existsPrependArg && cpsFun.isSync) {
                                    runFold = false
                                    if (!existsShiftedLambda && !cpsFun.isChanged && !unpure)
                                       CpsTree.pure(applyTerm)
                                    else
                                       buildApply(cpsFun, fun, applyRecords, applyTerm, existsAsyncArg, existsShiftedLambda, tails)
                                 } else {
                                    buildApply(cpsFun, fun, applyRecords, applyTerm, existsAsyncArg, existsShiftedLambda, tails)
                                 }
           if cpsCtx.flags.debugLevel >= 15 then
               cpsCtx.log(s"handleArgs: runFold=$runFold")
               cpsCtx.log(s"handleArgs: lastCpsTree=$lastCpsTree")
           if (runFold)
              val retval = (applyRecords::tails).foldRight(lastCpsTree){(pa,sa) =>
                 pa.foldRight(sa){ (p,s) =>
                   if (p.usePrepend(existsAsyncArg))
                      p.append(s)
                   else
                      s
                 }
              }
              if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"handleArgs: runFold result = ${retval}")
              retval
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
            val accRepeated = O.buildApplyArgsRecordsAcc(fun, paramsDescriptor,
                               rargs, cpsCtx.nestSame(TCM.Repeated),
                               acc.copy(inRepeat=true,records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList)
            acc.advance(nextRecord).copy(posIndex = accRepeated.posIndex)
         case r@Repeated(rargs, tpt) =>
            val accRepeated = O.buildApplyArgsRecordsAcc(fun, paramsDescriptor,
                               rargs, cpsCtx.nestSame(TCM.Repeated),
                               acc.copy(inRepeat=true, records=IndexedSeq.empty))
            val nextRecord = ApplyArgRepeatRecord(r, acc.posIndex, accRepeated.records.toList)
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
                val symbol = Symbol.newVal(Symbol.currentOwner,argName,t.tpe.widen,Flags.EmptyFlags,Symbol.noSymbol)
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

  def findAsyncShiftTerm(e:Term):ImplicitSearchResult =
    val tpe = e.tpe.widen
    val asyncShift = TypeIdent(Symbol.classSymbol("cps.AsyncShift")).tpe
    val tpTree = asyncShift.appliedTo(tpe)
    searchImplicit(tpTree)

  def findObjectAsyncShiftTerm(e:Term):ImplicitSearchResult =
    val tpe = e.tpe.widen
    val objAsyncShift = TypeIdent(Symbol.classSymbol("cps.ObjectAsyncShift")).tpe
    val tpTree = objAsyncShift.appliedTo(tpe)
    //val tpTree = Type.of[ObjectAsyncShift].appliedTo(tpe).simplified
    if cpsCtx.flags.debugLevel >= 15 then
      cpsCtx.log(s"searchImplicits: tpTree=$tpTree")
      cpsCtx.log(s"tpe=$tpe")
      cpsCtx.log(s"Type.of[ObjectAsyncShift]=${Type.of[ObjectAsyncShift]}")
    searchImplicit(tpTree)



  def shiftedApply(term: Term, args: List[Term], shiftedIndexes:Set[Int]): Term =


    def shiftSelectTypeApply(x:Select, targs:List[TypeTree]): Term =

       def withTargs(t:Term):Term =
         if (targs.isEmpty) 
           t
         else 
           TypeApply(t, targs)

       val qual = x.qualifier
       val monad = cpsCtx.monad.unseal
       if (qual.tpe <:< '[cps.runtime.CallChainAsyncShiftSubst[F,_]].unseal.tpe) then
          println("!!!shiftedEmpty-here")
          val shiftedName = x.name + "_shifted"
          qual.tpe.typeSymbol.method(shiftedName) match
            case Nil  => 
               throw MacroError(s"Can't find method ${shiftedName} for qual=${qual} ",posExpr(x))
            case m::Nil =>
               withTargs(Select(qual,m))
            case other =>
               Select.overloaded(qual,shiftedName,targs.map(_.tpe),args) match
                  case Apply(x,y) => x
                  case _ =>
                    throw MacroError(s"Overloaded method ${shiftedName} for qual=${qual} is not apply ",posExpr(x))
       else
         findObjectAsyncShiftTerm(qual) match
           case success1: ImplicitSearchSuccess =>
             val objectShift = success1.tree
             if (cpsCtx.flags.debugLevel >= 15)
               cpsCtx.log(s"found objectShift, ${success1.tree}")
               cpsCtx.log(s"objectShift.tpe = ${objectShift.tpe}")
             val shiftedExpr = Apply(
                               TypeApply(Select.unique(objectShift, "apply"),fType.unseal::Nil),
                               List(qual,monad)
                             )
             val newSelect = Select.unique(shiftedExpr, x.name)
             withTargs(newSelect)
           case failure1: ImplicitSearchFailure =>
             findAsyncShiftTerm(qual) match
               case success2: ImplicitSearchSuccess =>
                 val shiftType = success2.tree.tpe
                 val shiftSymbol = if (shiftType.isSingleton) {
                                     shiftType.termSymbol
                                   } else {
                                     shiftType.typeSymbol
                                   }
                 shiftSymbol.method(x.name) match
                    case Nil =>
                        throw MacroError(s"Method (${x.name}) is not defined in [${shiftType.show}], qual=${qual} ",posExpr(x))
                    case m::Nil =>
                        val newSelect = Select.unique(success2.tree, x.name)
                        TypeApply(newSelect, fType.unseal::targs).appliedTo(qual,monad)
                    case other =>
                        Select.overloaded(success2.tree, x.name, (fType.unseal::targs).map(_.tpe), List(qual,monad))
               case failure2: ImplicitSearchFailure =>
                 throw MacroError(s"Can't find AsyncShift (${failure2.explanation}) or ObjectAsyncShift (${failure1.explanation}) for qual=${qual} ",posExpr(x))


    def shiftCaller(term:Term): Term =
       if (cpsCtx.flags.debugLevel >= 15)
           cpsCtx.log(s"shiftCaller, t=$term")
       val monad = cpsCtx.monad.unseal
       term match
          case TypeApply(s@Select(qual,name),targs) =>
                  if (qual.tpe =:= monad.tpe)
                    if (s.symbol == mapSymbol || s.symbol == flatMapSymbol)
                      // TODO: add more symbols
                      findAsyncShiftTerm(qual) match
                        case shiftQual:ImplicitSearchSuccess =>
                          val newSelect = Select.unique(shiftQual.tree,s.name)
                          TypeApply(newSelect, targs).appliedTo(qual)
                        case failure:ImplicitSearchFailure =>
                          throw new MacroError(s"Can't find asyn shift for cpsMonad: ${failure.explanation}", posExpr(term))
                    else
                      throw new MacroError("Unimplemented shift for CpsMonad", posExpr(term))
                  else
                    shiftSelectTypeApply(s, targs)
          case s@Select(qual,name) =>
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

    val shiftedCaller = shiftCaller(term)
    if (shiftedCaller.symbol.isDefDef &&
        shiftedCaller.symbol.flags.is(Flags.Inline)
        ) {
      println("shiftedCaller is inline")
      println("defdefTree = "+shiftedCaller.symbol.tree)
    }
    
    Apply(shiftedCaller,args)

  end shiftedApply

  def buildApply(cpsFun: CpsTree, fun: Term,
                 argRecords: Seq[ApplyArgRecord],
                 applyTerm: Term,
                 withAsync: Boolean,
                 withShiftedLambda: Boolean,
                 tails: List[Seq[ApplyArgRecord]]
                 ): CpsTree =
        if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"buildApply: fun=${safeShow(fun)}")
            cpsCtx.log(s"cpsFun.isSync=${cpsFun.isSync}, withShiftedLambda=${withShiftedLambda}")
        val applyTpe = applyTerm.tpe
        if (withShiftedLambda)
          if (cpsFun.isSync)
            val shifted = buildShiftedApply(fun, argRecords, withAsync, tails)
            if (cpsCtx.flags.debugLevel >= 15)
               cpsCtx.log(s"buildApply: shifted=${safeShow(shifted)}")
            shiftedResultCpsTree(applyTerm, shifted)
          else
             //val sym = Symbol.newVal(Symbol.currentOwner, "x", t.widen, Flags.EmptyFlags, Symbol.noSymbol)   
             //val shifted = buildShiftedApply(Ref(x), argRecords, withAsync, tails)
             // TODO: monadFlatMap should accept lambda-expression
             cpsFun.monadFlatMap(v => buildShiftedApply(v,argRecords, withAsync, tails), applyTpe)
        else
          val args = argRecords.map(_.identArg(withAsync)).toList
          val tailArgss = tails.map(_.map(_.identArg(withAsync)).toList)
          val argss = args::tailArgss
          cpsFun match
             case lt:AsyncLambdaCpsTree =>
                     CpsTree.impure(lt.rLambda.appliedToArgss(argss), applyTpe)
             case _ =>
                    if (cpsFun.isSync)
                       cpsFun.applyTerm1(x => x.appliedToArgss(argss), applyTpe)
                    else
                       cpsFun.monadMap(x => x.appliedToArgss(argss), applyTpe)


  def shiftedResultCpsTree(origin: Term, shifted: Term): CpsTree =
      if (shifted.tpe.isFunctionType) 
         // TODO: extract argument types. now - one experiment
         shifted.tpe match
           case AppliedType(f,List(a,AppliedType(m,b))) if f <:< '[Function1].unseal.tpe =>
             val sym = Symbol.newVal(Symbol.currentOwner, "shiftedArg", a.widen, Flags.EmptyFlags, Symbol.noSymbol)
             AsyncLambdaCpsTree(origin, List(ValDef(sym,None)), 
                  CpsTree.impure(Apply(Select.unique(shifted,"apply"),List(Ref(sym))),b.head),origin.tpe)
           case _ =>
             throw MacroError("Async function with arity != 1 is not supported yet",posExprs(shifted,origin))
      else if (shifted.tpe <:< '[cps.runtime.CallChainAsyncShiftSubst[${fType},?]].unseal.tpe ) 
         println(s"!!!here, shifted.tpe=${shifted.tpe.show}")
         CallChainSubstCpsTree(origin, shifted, origin.tpe)
      else
         CpsTree.impure(shifted, origin.tpe)

 


  def buildShiftedApply(fun: Term,
                        argRecords:Seq[ApplyArgRecord],
                        withAsync: Boolean,
                        tails:List[Seq[ApplyArgRecord]]): Term =
      val shiftedIndexes = argRecords.zipWithIndex.filter(_._1.hasShiftedLambda).map(_._2)
      val args = argRecords.map(_.shift().identArg(withAsync)).toList
      if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"buildShiftedApply::argRecords=${argRecords}")
          cpsCtx.log(s"buildShiftedApply::args=${args}")
          cpsCtx.log(s"buildShiftedApply::tails=${tails}")
      val shiftedApplyHead = shiftedApply(fun, args, shiftedIndexes.toSet)
      val shiftedTails = tails.map(_.map(_.shift().identArg(withAsync)).toList)
      val r = shiftedApplyHead.appliedToArgss(shiftedTails)
      if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"buildShiftedApply::r.tpe=${r.tpe}")
      r


object ApplyTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: QuoteContext)(cpsCtx1: TransformationContext[F,T],
                         applyTerm: qctx1.tasty.Term,
                         fun: qctx1.tasty.Term,
                         args: List[qctx1.tasty.Term]): CpsExpr[F,T] = {
     //val tmpCpsCtx = cpsCtx
     val tmpQctx = qctx1
     val tmpFtype = summon[Type[F]]
     val tmpCTtype = summon[Type[T]]
     class Bridge extends TreeTransformScope[F,T]
                                                    {
                                                    //with TreeTransformScopeInstance[F,T](tc)(summon[Type[F]], summon[Type[T]], qctx) {

         implicit val qctx = qctx1
         implicit val fType = tmpFtype
         implicit val ctType = tmpCTtype

         val cpsCtx = cpsCtx1

         def bridge(): CpsExpr[F,T] =
            val treeResult = runApply(applyTerm.asInstanceOf[qctx.tasty.Term],
                                fun.asInstanceOf[qctx.tasty.Term],
                                args.asInstanceOf[List[qctx.tasty.Term]],
                                Nil
                             )
            try
              val exprResult = treeResult.toResult[T]
              exprResult
            catch
              case ex: Throwable =>
                ex.printStackTrace()
                throw ex

     }
     (new Bridge).bridge()
  }


