package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._

trait ApplyTreeTransform[F[_],CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F,CT, CC] =>

  import qctx.reflect._


  /**
   * case Apply(fun,args)
   *  transform application
   *
   * tails -- list of prepared sequence of curried arguments.
   **/
  def runApply(applyTerm: Apply,
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
             val (awaitable, monadContext) = args match
               case List(frs, snd) => (frs, snd)
               case other =>
                  throw MacroError(s"expected that await have two implicit argument, our args:${args}", posExprs(fun, applyTerm))
             runAwait(applyTerm, args1.head, targs2.head.tpe, awaitable, monadContext)
       case Apply(fun1, args1) =>
            handleFunApply(applyTerm, fun, args, fun1, args1, tails)
       case _ =>
            handleFun(applyTerm, fun, args, tails)
     if (cpsCtx.flags.debugLevel > 15)
       cpsCtx.log(s"runApply result = ${r}")
       cpsCtx.log(s"runApply result transformed = ${safeShow(r.transformed)}")
     r

  def sameSelect(funTerm:Term, name:String, targs:List[TypeTree], args:List[Term]):Option[Term] =
      if (cpsCtx.flags.debugLevel >= 15 && name=="apply")
          println(s"sameSelect: funTerm=${funTerm}")
          println(s"sameSelect: funTerm.tpe.typeSymbol=${funTerm.tpe.typeSymbol}")
      funTerm.tpe.typeSymbol.memberMethod(name) match
        case Nil => None
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
  def handleFunTypeApply(applyTerm: Apply,
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
          val cpsObj1 = runRoot(obj1)
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
                               val cpsObj = cpsObj1.select(obj, obj.symbol, obj.tpe).typeApply(fun, targs, fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj, args, tails)
                            case Some(term) =>
                               // for now, will check both term and tree. TODO build CpsTree in sameSelect
                               handleArgs1(applyTerm, term, CpsTree.pure(term, isChanged=true), args, tails, unpure=true)
                 case _ =>
                     val cpsObj = cpsObj1.select(obj, obj.symbol, obj.tpe).typeApply(fun, targs, fun.tpe)
                     handleArgs1(applyTerm, fun, cpsObj, args, tails)
          else if (cpsObj1.isChanged)
              //val cpsObj = cpsObj1.applyTerm1(x=>TypeApply(Select(x,obj.symbol),targs), fun.tpe)
              val cpsObj = cpsObj1.select(obj, obj.symbol, obj.tpe).typeApply(fun, targs, fun.tpe)
              handleArgs1(applyTerm, fun, cpsObj, args, tails)
          else
              handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)
        case Ident(name) =>
          handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)
        case _ =>
          val cpsObj = runRoot(obj)
          handleArgs1(applyTerm, fun, cpsObj, args, tails)
     }


  def handleFunSelect(applyTerm:Apply,
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
       case conv@Inlined(_,_,
              Typed(
                 Lambda(List(xValDef),
                   Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1))),
                 cv)
              ) if (obj3.symbol == awaitSymbol
                   && xValDef.symbol == x.symbol) =>
                  // here we catch await, inserted by implicit conversion.
                  // this code is likey depends from implementation details of a compiler
                  // mb create compiler-level API ?
                  withInlineBindings(conv, runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head))
       case conv@Inlined(_,_,
                 Lambda(List(xValDef),
                   Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1)))
            ) if (obj3.symbol == awaitSymbol
                   && xValDef.symbol == x.symbol) =>
                  // transient inlines have no 'Typed' entry
                  //  TODO: handle non-inlined conversion
                  withInlineBindings(conv,runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head))
       case _ =>
         val cpsObj = runRoot(obj)
         if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"funSelect: cpsObj=${cpsObj}")
         cpsObj match
            case lt: AsyncLambdaCpsTree =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: AsyncLambdaCpsTree discovered, fun=$fun fun.tpe=${fun.tpe}")
               handleArgs1(applyTerm, fun, cpsObj.select(fun, fun.symbol, fun.tpe), args, tails)
            case cls: CallChainSubstCpsTree =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: CallChainSubstCpsTree discovered, fun=$fun fun.tpe=${fun.tpe}")
                  cpsCtx.log(s"funSelect: cls.shifted = ${cls.shifted.show}")
               sameSelect(cls.shifted, methodName, List.empty, args) match 
                  case None => 
                               if (cpsCtx.flags.debugLevel >= 15) then
                                  cpsCtx.log(s"not found name ${methodName} for ${cls.shifted.show}")
                               val cpsObj1 = cpsObj.select(fun,fun.symbol, fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails)
                  case Some(term) => 
                               if (cpsCtx.flags.debugLevel >= 15) then
                                  cpsCtx.log(s"found sameSelect: ${term.show} ")
                               val cpsObj1 = CpsTree.pure(term, isChanged = true)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails, unpure=true)
            case _ =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: ! lambda || Subst, fun=$fun fun.tpe=${fun.tpe}")
               handleArgs1(applyTerm, fun, cpsObj.select(fun, fun.symbol, fun.tpe), args, tails)

  def withInlineBindings(origin: Inlined, tree:CpsTree):CpsTree =
        if (origin.bindings.isEmpty)
           tree
        else
           InlinedCpsTree(origin, origin.bindings, tree)

  def handleFunIdent(applyTerm: Apply, fun:Term, args:List[Term], name: String, tails: List[Seq[ApplyArgRecord]]):CpsTree =
        handleArgs1(applyTerm, fun, CpsTree.pure(fun), args, tails)

  def handleFunApply(applyTerm: Apply, fun:Term, args: List[Term],
                                      fun1: Term, args1: List[Term],
                                      tails: List[Seq[ApplyArgRecord]]):CpsTree =
        val paramsDescriptor = MethodParamsDescriptor(fun)
        val argsRecords = O.buildApplyArgsRecords(paramsDescriptor, args, cpsCtx)
        runApply(applyTerm, fun1, args1, argsRecords::tails)


  def handleFun(applyTerm: Apply, fun:Term, args:List[Term], tails: List[Seq[ApplyArgRecord]]):CpsTree =
       val cpsFun = runRoot(fun)
       handleArgs1(applyTerm, fun, cpsFun, args, tails)



  def shiftedLambdaTypeTree(tpt: TypeTree): TypeTree =
    Inferred(shiftedLambdaType(tpt.tpe))

  def shiftedLambdaType(tpe: TypeRepr): TypeRepr =
    tpe.widen match {
      case MethodType(paramNames, paramTypes, resType) =>
               // currently no support for path-dependend lambdas.
               MethodType(paramNames)( mt => paramTypes,
                                       mt => TypeRepr.of[F].appliedTo(resType))
      case PolyType(paramNames,paramBounds,resType) =>
               PolyType(paramNames)(pt => paramBounds,
                                    pt => TypeRepr.of[F].appliedTo(resType))
      case _ => throw MacroError(s"Not supported type for shifting: ${tpe.show}",cpsCtx.patternCode)
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
  def handleArgs1(applyTerm: Apply,
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


        val paramsDescriptor = MethodParamsDescriptor(fun)

        val applyRecords = O.buildApplyArgsRecords(paramsDescriptor, args, cpsCtx)

        val argsProperties = ApplyArgsSummaryProperties.mergeSeqSeq(applyRecords::tails)

        val existsAsyncArg = argsProperties.hasAsync
        val existsPrependArg = argsProperties.usePrepend
        val existsShiftedLambda = argsProperties.hasShiftedLambda
        val shouldBeChangedSync = argsProperties.shouldBeChangedSync
        if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s" existsShiftedLambda=${existsShiftedLambda}")
            cpsCtx.log(s" existsAsyncArg=${existsAsyncArg}")
            cpsCtx.log(s" existsPrependArg=${existsPrependArg}")
            cpsCtx.log(s" shouldBeChangedSync=${shouldBeChangedSync}")

        if (!existsAsyncArg && !existsShiftedLambda && !shouldBeChangedSync) {
           val tailArgss = tails.map(_.map(_.term).toList)
           cpsFun match
              case lt: AsyncLambdaCpsTree =>
                      CpsTree.impure(Select.unique(lt.rLambda,"apply").appliedToArgss(args::tailArgss), applyTerm.tpe)
              case _ =>
                      cpsFun.syncOrigin match
                         case Some(fun1) =>
                           if (!cpsFun.isChanged && !unpure)
                             CpsTree.pure(applyTerm)
                           else
                             if (unpure)
                                val internalApply = fun1.appliedToArgss(args::tailArgss)
                                shiftedResultCpsTree(applyTerm, internalApply)
                             else
                                CpsTree.pure(fun1.appliedToArgss(args::tailArgss), true)
                         case _ =>
                            cpsFun.monadMap(x => x.appliedToArgss(args::tailArgss), applyTerm.tpe)
        } else {
           var runFold = true
           val lastCpsTree: CpsTree = if (!existsPrependArg && cpsFun.isSync) {
                                    runFold = false
                                    if (!existsShiftedLambda && !cpsFun.isChanged && 
                                        !unpure && !shouldBeChangedSync)
                                       CpsTree.pure(applyTerm)
                                    else
                                       buildApply(cpsFun, fun, applyRecords, applyTerm, argsProperties, unpure, tails)
                                 } else {
                                    buildApply(cpsFun, fun, applyRecords, applyTerm, argsProperties, unpure, tails)
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


  def findAsyncShiftTerm(e:Term):(ImplicitSearchResult, TypeRepr) =
    val tpe = e.tpe.widen
    val asyncShift = TypeIdent(Symbol.classSymbol("cps.AsyncShift")).tpe
    val asyncShiftType = asyncShift.appliedTo(tpe)
    (Implicits.search(asyncShiftType), asyncShiftType)



  def shiftedApplyCps(
                      cpsTree: CpsTree, 
                      originArgs:List[Term], 
                      shiftedArgs: List[Term], 
                      shiftedIndexes:Set[Int], //TODO: this should be List[Set[Int]]
                      shiftedTails: List[List[Term]],
                      applyTerm: Term): CpsTree =

    def condTypeApply(sel:Term, targs: List[TypeTree]):Term =
      if (targs.isEmpty) sel else TypeApply(sel,targs)

    cpsTree.syncOrigin match
      case Some(origin) =>
        val head = shiftedApplyTerm(origin, originArgs, shiftedArgs, shiftedIndexes)
        shiftedResultCpsTree(applyTerm, head.appliedToArgss(shiftedTails))
      case None =>
        cpsTree match
           case _ : PureCpsTree  |  EmptyCpsTree => 
              // impossible
              val originTerm = cpsTree.syncOrigin.get
              val head = shiftedApplyTerm(originTerm, originArgs, shiftedArgs, shiftedIndexes)
              shiftedResultCpsTree(applyTerm, head.appliedToArgss(shiftedTails))
           case SelectTypeApplyCpsTree(optOrigin,nested,targs,selects,otpe, changed) =>
              if (selects.isEmpty) then
                 if (targs.isEmpty) then
                   shiftedApplyCps(nested,originArgs,shiftedArgs,shiftedIndexes,shiftedTails,applyTerm)
                 else
                   // this can be only generice async-lambda
                   //  which is impossible to create in scala syntax
                   throw MacroError("can't shift async head: ${cpsTree}", posExprs(applyTerm))
              else
                 val current = selects.head
                 val prevSelects = selects.tail
                 val prev = SelectTypeApplyCpsTree.create(optOrigin,nested,targs,prevSelects,current.prevTpe, changed)
                 prev.monadFlatMap({x => 
                       val funToShift = condTypeApply(Select(x,current.symbol),current.targs)
                       val head = shiftedApplyTerm(funToShift,originArgs,shiftedArgs,shiftedIndexes)
                       head.appliedToArgss(shiftedTails)
                 }, applyTerm.tpe)
           case lt@AsyncLambdaCpsTree(originLambda,params,body,otpe) =>
              val head = shiftedApplyTerm(lt.rLambda, originArgs, shiftedArgs, shiftedIndexes)
              shiftedResultCpsTree(applyTerm, head.appliedToArgss(shiftedTails))
           case x: AsyncCpsTree =>
              // this can be only function, so, try to call apply method
              x.monadMap({fun =>
                  val head = shiftedApplyTerm(Select.unique(fun,"apply"), originArgs, shiftedArgs, shiftedIndexes)
                  head.appliedToArgss(shiftedTails)
              }, applyTerm.tpe)
           case BlockCpsTree(stats, last) =>
                  BlockCpsTree(stats,shiftedApplyCps(last,originArgs,shiftedArgs,shiftedIndexes,
                               shiftedTails, applyTerm))
           case InlinedCpsTree(origin, bindings, nested) =>
                  InlinedCpsTree(origin, bindings, shiftedApplyCps(nested, originArgs, shiftedArgs,
                                         shiftedIndexes, shiftedTails, applyTerm))
           case ValCpsTree(valDef, rightPart, nested, canBeLambda) =>
                  ValCpsTree(valDef, rightPart, shiftedApplyCps(nested, originArgs, shiftedArgs,
                                         shiftedIndexes, shiftedTails, applyTerm), canBeLambda)
           case AppendCpsTree(frs, snd) =>
                  AppendCpsTree(frs, 
                      shiftedApplyCps(snd,originArgs,shiftedArgs,shiftedIndexes,shiftedTails,applyTerm))
           case CallChainSubstCpsTree(origin, shifted, otpe) =>
                  val head = shiftedApplyTerm(Select.unique(shifted,"apply"), originArgs, shiftedArgs, shiftedIndexes)
                  shiftedResultCpsTree(applyTerm, head.appliedToArgss(shiftedTails))
                  
                  

  def shiftedApplyTerm(term: Term, originArgs:List[Term], shiftedArgs: List[Term], shiftedIndexes:Set[Int]): Term =

    val monad = cpsCtx.monad.asTerm

    def checkInplaceAsyncMethodCandidate(methodSym: Symbol, qual: Term, targs: List[TypeTree], args: List[Term]): Either[MessageWithPos,Boolean] =
       val paramSymss = methodSym.paramSymss
       val mPos = methodSym.pos.getOrElse(qual.pos)
       if (paramSymss.isEmpty) then
           Left(MessageWithPos(s"${methodSym.name} for qual=${qual} should have arguments",mPos))
       else
           val typeArgsShifted = paramSymss.head.filter(_.isType)
           val nTypeArgsShifted = typeArgsShifted.length
           val nTypeArgs = targs.length
           if (nTypeArgs != nTypeArgsShifted) && (nTypeArgsShifted != nTypeArgs + 1) then 
              Left(MessageWithPos(s" ${methodSym.name} for qual=${qual} number of type arguments is impossible",mPos))
           else 
              // TODO: additional check ?
              Right(nTypeArgsShifted == nTypeArgs + 1)

    def findInplaceAsyncMethodCall(x:Select, shiftedName: String, 
                                  targs: List[TypeTree], args: List[Term]): Either[List[MessageWithPos],Term] =
         val qual = x.qualifier

         def withTargs(t:Term):Term =
           if (targs.isEmpty) 
             t
           else 
             TypeApply(t, targs)

         qual.tpe.typeSymbol.memberMethod(shiftedName) match
           case Nil =>
             Left(List())
           case m::Nil =>
             checkInplaceAsyncMethodCandidate(m, qual, targs, args) match
               case Left(error) => Left(List(error))
               case Right(useExtraArgs) =>
                 if (useExtraArgs)
                   Right(Apply(TypeApply(Select.unique(qual,shiftedName), TypeTree.of[F]::targs), monad::args))
                 else
                   Right(Apply(withTargs(Select.unique(qual,shiftedName)), args))
           case overloaded =>
               var errors: List[MessageWithPos] = List.empty
               var foundUseExtra: List[Symbol] = List.empty
               var foundDirect: List[Symbol] = List.empty
               var c = overloaded
               while(!c.isEmpty) {
                 val sym = c.head
                 c = c.tail
                 checkInplaceAsyncMethodCandidate(sym, qual, targs, args) match
                   case Left(e) => errors = e::errors
                   case Right(useExtra) =>
                      if (useExtra)
                        foundUseExtra = sym::foundUseExtra
                      else
                        foundDirect = sym::foundDirect
               }
               if (foundDirect.isEmpty) then
                   if (foundUseExtra.isEmpty) then
                      Left(errors)
                   else
                      // TODO: add retval to overload check
                      Right(Select.overloaded(qual,shiftedName,TypeTree.of[F].tpe::targs.map(_.tpe), monad::args))
               else 
                   if (foundUseExtra.isEmpty) then
                      Right(Select.overloaded(qual,shiftedName,targs.map(_.tpe), args))
                   else
                      // found both variants, can't choose ?
                      Left(List(
                        MessageWithPos("More than one candidate for overloaded variant for method $shiftedName, qual=${qual.show}",x.pos)
                      ))
            

    def shiftSelectTypeApplyApply(x: Select, targs: List[TypeTree], args: List[Term]): Term =
       x.qualifier match
         case qual@Apply(Select(col,"withFilter"),List(predicate)) if (
                          qual.tpe <:< TypeRepr.of[scala.collection.WithFilter[?,?]]) =>
            val (colAsyncShiftSearch, askedShiftedType) = findAsyncShiftTerm(col)
            colAsyncShiftSearch match
              case success: ImplicitSearchSuccess =>
                 val csf = success.tree
                 val withFilterSubstSelect = Select.unique(csf,"_cpsWithFilterSubst")
                 val newQual = Apply(withFilterSubstSelect,List(col,predicate))       
                 val newSelect = Select.unique(newQual,x.name)
                 TypeApply(newSelect, TypeTree.of[F]::targs).appliedTo(monad).appliedToArgs(args)
              case failure: ImplicitSearchFailure =>
                 throw MacroError(s"Can't resolve [${askedShiftedType.show}] when parsing withFilter ",posExpr(col))
         case _ =>
            shiftSelectTypeApplyApplyClear(x, targs, args)

                        
       
    
    def shiftSelectTypeApplyApplyClear(x:Select, targs:List[TypeTree], args: List[Term]): Term =

       val qual = x.qualifier

       def traceFunNotFound(msg:String, errors:List[MessageWithPos]): Unit =
          if (!errors.isEmpty) then
            report.warning(msg)
            errors.foreach(e =>
              report.warning(e.message)
            )

       val shiftedName = x.name + "_async"  
       findInplaceAsyncMethodCall(x, shiftedName,  targs, args) match
         case Right(t) => t
         case Left(funErrors) => 
           val funErrors0 = funErrors
           val shiftedName1 = x.name + "Async"
           findInplaceAsyncMethodCall(x, shiftedName1,  targs, args) match
             case Right(t) => t
             case Left(funErrors) =>
               val (asyncShiftSearch, askedShiftedType) = findAsyncShiftTerm(qual)
               asyncShiftSearch match
                 case success2: ImplicitSearchSuccess =>
                   val shiftType = success2.tree.tpe
                   val shiftSymbol = if (shiftType.isSingleton) {
                                     shiftType.termSymbol
                                   } else {
                                     shiftType.typeSymbol
                                   }
                   shiftSymbol.memberMethod(x.name) match
                    case Nil =>
                        throw MacroError(s"Method (${x.name}) is not defined in [${shiftType.show}], qual=${qual} ",posExpr(x))
                    case m::Nil =>
                        val newSelect = Select.unique(success2.tree, x.name)
                        TypeApply(newSelect, TypeTree.of[F]::targs).appliedTo(qual,monad).appliedToArgs(args)
                    case other =>
                        // TODO: other args in typebounds [?]
                        val expectedType = TransformUtil.createFunctionType(using qctx)(shiftedArgs.map(_.tpe), TypeBounds.empty)
                        val shiftedCaller = Select.overloaded(success2.tree, x.name, (TypeTree.of[F]::targs).map(_.tpe), List(qual,monad), expectedType)
                        Apply(shiftedCaller, args)
                             
                 case failure2: ImplicitSearchFailure =>
                   traceFunNotFound(s"failed candidates for ${qual.show} ${shiftedName}",funErrors)
                   if cpsCtx.flags.debugLevel >= 15 then
                      for((a,i) <- args.zipWithIndex) {
                        cpsCtx.log(s"arg($i)=${a.show}")
                      }
                   throw MacroError(s"Can't find AsyncShift (${failure2.explanation}) or async functions) for qual=${qual} name = ${x.name}, shiftedName=${shiftedName}, askedShiftedType=${askedShiftedType.show}",posExpr(x))


    if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"shiftCaller, t=$term")
    term match
       case TypeApply(s@Select(qual,name),targs) =>
                  shiftSelectTypeApplyApply(s, targs, shiftedArgs)
       case s@Select(qual,name) =>
                    shiftSelectTypeApplyApply(s, Nil, shiftedArgs)
       //case TypeApply(x, targs) =>  // now scala hvw no multiple type params
       //           Apply(TypeApply(shiftCaller(x),targs),args)
       case Lambda(params, body) =>
                  val shiftedSymbols = params.zipWithIndex.filter{
                      (p,i) => shiftedIndexes.contains(i)
                  }.map{ (p,i) => p.symbol }.toSet
                  val nBody = asyncShift(body, shiftedSymbols)
                  ???
       case Block(statements, last) =>
                  // TODO: change cpsFun appropriative
                  Block(statements, shiftedApplyTerm(last, originArgs, shiftedArgs, shiftedIndexes))
       case _ =>
                  report.warning(s"""
                     Need to shift $term, tpe.widen=${term.tpe.widen} 
                     originArgs=${originArgs}
                     shiftedArgs=${shiftedArgs}
                  """, posExprs(term))
                  throw MacroError(s"Can't shift caller ${term}",posExprs(term))

  end shiftedApplyTerm

  def buildApply(cpsFun: CpsTree, fun: Term,
                 argRecords: Seq[ApplyArgRecord],
                 applyTerm: Apply,
                 argsProperties: ApplyArgsSummaryProperties,
                 inShiftedCallChain: Boolean,
                 tails: List[Seq[ApplyArgRecord]]
                 ): CpsTree =
        if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"buildApply: fun=${safeShow(fun)}")
            cpsCtx.log(s"buildApply: cpsFun.isSync=${cpsFun.isSync}, withShiftedLambda=${argsProperties.hasShiftedLambda}, inShiftedCallChain=${inShiftedCallChain}")
        val withAsync = argsProperties.hasAsync
        val applyTpe = applyTerm.tpe
        if (argsProperties.hasShiftedLambda)
          buildShiftedApply(cpsFun, fun, argRecords, withAsync, tails, applyTerm)
        else
          val args = argRecords.map(_.identArg(withAsync)).toList
          val tailArgss = tails.map(_.map(_.identArg(withAsync)).toList)
          val argss = args::tailArgss
          val retval = cpsFun match
             case lt:AsyncLambdaCpsTree =>
                    CpsTree.impure(lt.rLambda.appliedToArgss(argss), applyTpe)
             case cs:CallChainSubstCpsTree =>
                    if (cpsCtx.flags.debugLevel >= 15) {
                       cpsCtx.log(s"buildApply: cs:CallChainSubstCpsTree")
                    }
                    shiftedResultCpsTree(applyTerm, cs.shifted.appliedToArgss(argss))
             case _ =>
                    cpsFun.syncOrigin match
                       case Some(fun) =>
                          val applied = fun.appliedToArgss(argss)
                          if (inShiftedCallChain)
                             shiftedResultCpsTree(applyTerm, applied)
                          else
                             CpsTree.pure(applied, isChanged=true)
                       case None =>
                          if (inShiftedCallChain)
                             val shifted = cpsFun.transformed
                             shiftedResultCpsTree(applyTerm, shifted.appliedToArgss(argss))
                          else
                             cpsFun.monadMap(x => x.appliedToArgss(argss), applyTpe)
          if (cpsCtx.flags.debugLevel >= 15) {
             cpsCtx.log(s"buildApply: retval = $retval")
          }
          retval


  def shiftedResultCpsTree(origin: Term, shifted: Term): CpsTree =
      
      if (shifted.tpe.isFunctionType)
         // TODO: extract argument types. now - one experiment
         shifted.tpe match
           case AppliedType(f,List(a,AppliedType(m,b))) if f <:< TypeRepr.of[Function1] =>
             val sym = Symbol.newVal(Symbol.spliceOwner, "shiftedArg", a.widen, Flags.EmptyFlags, Symbol.noSymbol)
             AsyncLambdaCpsTree(origin, List(ValDef(sym,None)),
                  CpsTree.impure(Apply.copy(origin)(Select.unique(shifted,"apply"),List(Ref(sym))),b.head),origin.tpe)
           case _ =>
             throw MacroError("Async function with arity != 1 is not supported yet",posExprs(shifted,origin))
      else if (shifted.tpe <:< TypeRepr.of[cps.runtime.CallChainAsyncShiftSubst[F,?,?]])
         CallChainSubstCpsTree(origin, shifted, origin.tpe)
      else if (shifted.tpe.widen =:= origin.tpe.widen) 
         // experimental, when shifted result type is the same as result type.
         CpsTree.pure(shifted, isChanged=true)
      else
         CpsTree.impure(shifted, origin.tpe)




  def buildShiftedApply(
                        cpsFun: CpsTree,
                        fun: Term,
                        argRecords:Seq[ApplyArgRecord],
                        withAsync: Boolean,
                        tails:List[Seq[ApplyArgRecord]],
                        applyTerm: Apply): CpsTree =
      val shiftedIndexes = argRecords.zipWithIndex.filter(_._1.hasShiftedLambda).map(_._2)
      val shiftedArgs = argRecords.map(_.shift().identArg(withAsync)).toList
      val originArgs = argRecords.map(_.term).toList
      if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"buildShiftedApply::fun=${fun}")
          cpsCtx.log(s"buildShiftedApply::argRecords=${argRecords}")
          cpsCtx.log(s"buildShiftedApply::shiftedArgs=${shiftedArgs}")
          cpsCtx.log(s"buildShiftedApply::tails=${tails}")
      val shiftedTails = tails.map(_.map(_.shift().identArg(withAsync)).toList)
      shiftedApplyCps(cpsFun, originArgs, shiftedArgs, shiftedIndexes.toSet, shiftedTails, applyTerm)


object ApplyTreeTransform:


  def run[F[_]:Type,T:Type,C<:CpsMonadContext[F]:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T,C],
                         applyTerm: qctx1.reflect.Apply,
                         fun: qctx1.reflect.Term,
                         args: List[qctx1.reflect.Term]): CpsExpr[F,T] = {
     //val tmpCpsCtx = cpsCtx
     val tmpQctx = qctx1
     val tmpFtype = summon[Type[F]]
     val tmpCTtype = summon[Type[T]]
     val tmpCCtype = summon[Type[C]]
     class Bridge extends TreeTransformScope[F,T,C]
                                                    {
                                                    //with TreeTransformScopeInstance[F,T](tc)(summon[Type[F]], summon[Type[T]], qctx) {

         implicit val qctx = qctx1
         implicit val fType = tmpFtype
         implicit val ctType = tmpCTtype
         implicit val ccType = tmpCCtype

         val cpsCtx = cpsCtx1

         def bridge(): CpsExpr[F,T] =
            val treeResult = runApply(applyTerm.asInstanceOf[qctx.reflect.Apply],
                                fun.asInstanceOf[qctx.reflect.Term],
                                args.asInstanceOf[List[qctx.reflect.Term]],
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


