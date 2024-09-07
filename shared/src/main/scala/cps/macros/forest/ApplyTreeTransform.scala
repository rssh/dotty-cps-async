package cps.macros.forest

import scala.annotation.nowarn
import scala.quoted.*
import scala.util.control.NonFatal
import cps.*
import cps.macros.*
import cps.macros.common.*
import cps.macros.misc.*
import cps.macros.forest.application.ApplicationShiftType


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
              tails: List[Seq[ApplyArgRecord]])(owner: Symbol): CpsTree =
     if (cpsCtx.flags.debugLevel >= 10)
       cpsCtx.log(s"runApply, appyTerm=${safeShow(applyTerm)}")
     val monad = cpsCtx.monad
     //if (fun.symbol == logicalAndSym || fun.symbol == logicalOrSym) {
     //   handleBooleanAndOr(fun,args)
     //} 
     // try to omit things, which should be eta-expanded,
     val r = fun match
       case TypeApply(obj,targs) if obj.symbol == nonLocalReturnsReturningSym =>
            runNonLocalReturnsReturning(applyTerm,fun,targs,args)(owner) 
       case TypeApply(obj,targs) =>
            handleFunTypeApply(applyTerm,fun,args,obj,targs, tails)(owner)
       case Select(obj,method) =>
            if (fun.symbol == logicalAndSym || fun.symbol == logicalOrSym) then
               handleBooleanAndOr(applyTerm, fun, obj, args)(owner)
            else
               handleFunSelect(applyTerm, fun, args, obj, method, tails)(owner)
       case Ident(name) =>
            handleFunIdent(applyTerm, fun, args, name, tails)(owner)
       case Apply(fun1@TypeApply(obj2,targs2), args1) if obj2.symbol == awaitSymbol =>
             // catch await early
             val (monadContext, conversion) = args match
               case List(frs, snd) => (frs, snd)
               case other =>
                  throw MacroError(s"expected that await have two implicit argument, our args:${args}", posExprs(fun, applyTerm))
             runAwait(applyTerm, args1.head, targs2.head.tpe, conversion, monadContext)(owner)
       case applyFun@Apply(TypeApply(obj2,targs2),args1) if obj2.symbol == nonLocalReturnsThrowReturnSym /*&& cpsCtx.inShiftedReturning*/ =>
             runNonLocalReturnsThrowReturn(applyTerm, applyFun, args1.head, targs2, args.head)(owner)
       case Apply(fun1, args1) =>
            handleFunApply(applyTerm, fun, args, fun1, args1, tails)(owner)
       case _ =>
            handleFun(applyTerm, fun, args, tails)(owner)
     if (cpsCtx.flags.debugLevel > 15)
       cpsCtx.log(s"runApply result = ${r}")
       cpsCtx.log(s"runApply result transformed = ${safeShow(r.transformed)}")
     r

  def sameSelect(funTerm:Term, name:String, targs:List[TypeTree], args:List[Term])(owner: Symbol):Option[Term] =
      if (cpsCtx.flags.debugLevel >= 15 && name=="apply")
          cpsCtx.log(s"sameSelect: funTerm=${funTerm.show}")
          cpsCtx.log(s"sameSelect: funTerm.tpe.typeSymbol=${funTerm.tpe.typeSymbol}")
      funTerm.tpe.typeSymbol.methodMember(name) match
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

  
  def handleBooleanAndOr(applyTerm: Apply, fun: Term, x: Term, args: List[Term])(owner: Symbol): CpsTree = {

    def shortcutOpConst(x:Term): (Term, Boolean) = {
      if (fun.symbol == logicalOrSym) {
          (x, true)
      } else {
          ('{ !${x.asExprOf[Boolean]} }.asTerm,  false)
          //(Apply(Select(x,logicalNotSym),List()), false)
      }
    }

    def genShortcutIf(xCond:Expr[Boolean], xShort: Boolean, y: Expr[F[Boolean]]): Expr[F[Boolean]] = {
      '{  if (${xCond}) {
              ${cpsCtx.monad}.pure(${Expr(xShort)})
          } else {
               ${y} 
          }
      }
    }

    args match
      case List(y) => 
         val xCpsTree = runRoot(x)(owner)
         val yCpsTree = runRoot(y)(owner)
         (xCpsTree.syncOrigin, yCpsTree.syncOrigin) match
            case (Some(xx), Some(yy)) =>
               if (!xCpsTree.isChanged  && !yCpsTree.isChanged) then
                  CpsTree.pure(owner, applyTerm, false)
               else
                  CpsTree.pure(owner,
                               Apply.copy(applyTerm)( 
                                 Select(xx,fun.symbol),
                                 List(yy)
                               )
                  )
            case (Some(xx), None) =>
               val (xOp, xCnt) = shortcutOpConst(x)
               val expr = genShortcutIf(xOp.asExprOf[Boolean], xCnt, yCpsTree.transformed.asExprOf[F[Boolean]])
               CpsTree.impure(owner, expr.asTerm, TypeRepr.of[Boolean])
            case (None, Some(yy)) =>
               xCpsTree.monadMap(t => Apply.copy(applyTerm)(Select(t,fun.symbol),List(yy)), TypeRepr.of[Boolean])
            case (None, None) =>
               xCpsTree.monadFlatMap({ t =>
                  val (tOp, tCnt) = shortcutOpConst(t)
                  genShortcutIf(tOp.asExprOf[Boolean], tCnt,
                                 yCpsTree.transformed.asExprOf[F[Boolean]]
                  ).asTerm
               }, TypeRepr.of[Boolean] )
      case _ =>   
         throw MacroError("expected that boolean operator should have one arguments", posExprs(applyTerm))
  }              

  /**
   *applyTerm = Apply(fun, args)
   *fun = TypeApply(obj,targs)
   **/
  def handleFunTypeApply(applyTerm: Apply,
                         fun:Term,
                         args: List[Term],
                         obj:Term,
                         targs:List[TypeTree],
                         tails:List[Seq[ApplyArgRecord]])(owner: Symbol): CpsTree =

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
          val cpsObj1 = runRoot(obj1)(owner)
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
                          sameSelect(shifted, method, targs, args)(owner) match
                            case None =>
                               // not-found, use origin
                               val cpsObj = cpsObj1.select(obj, obj.symbol, obj.tpe).typeApply(fun, targs, fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj, args, tails)(owner)
                            case Some(term) =>
                               // for now, will check both term and tree. TODO build CpsTree in sameSelect
                               handleArgs1(applyTerm, term, CpsTree.pure(owner, term, isChanged=true), args, tails, unpure=true)(owner)
                 case _ =>
                     val cpsObj = cpsObj1.select(obj, obj.symbol, obj.tpe).typeApply(fun, targs, fun.tpe)
                     handleArgs1(applyTerm, fun, cpsObj, args, tails)(owner)
          else if (cpsObj1.isChanged)
              //val cpsObj = cpsObj1.applyTerm1(x=>TypeApply(Select(x,obj.symbol),targs), fun.tpe)
              val cpsObj = cpsObj1.select(obj, obj.symbol, obj.tpe).typeApply(fun, targs, fun.tpe)
              handleArgs1(applyTerm, fun, cpsObj, args, tails)(owner)
          else
              handleArgs1(applyTerm, fun, CpsTree.pure(owner, fun), args, tails)(owner)
        case Ident(name) =>
          handleArgs1(applyTerm, fun, CpsTree.pure(owner, fun), args, tails)(owner)
        case _ =>
          //
          val cpsFun = runRoot(fun)(owner)
          handleArgs1(applyTerm, fun, cpsFun, args, tails)(owner)
     }


  def handleFunSelect(applyTerm:Apply,
                      fun:Term,
                      args:List[Term],
                      obj:Term,
                      methodName: String,
                      tails: List[Seq[ApplyArgRecord]])(owner: Symbol): CpsTree =
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
                  val monadContext = args1.head
                  val monadConversion = args1.tail.head
                  // here we catch await, inserted by implicit conversion.
                  // this code is likey depends from implementation details of a compiler
                  // mb create compiler-level API ?
                  withInlineBindings(owner, conv, 
                    runAwait(applyTerm, args.head, targs3.head.tpe,monadConversion, monadContext)(owner))
       case conv@Inlined(_,_,
                 Lambda(List(xValDef),
                   Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1)))
            ) if (obj3.symbol == awaitSymbol
                   && xValDef.symbol == x.symbol) =>
                  // transient inlines have no 'Typed' entry
                  //  TODO: handle non-inlined conversion
                  val monadContext = args1.head
                  val monadConversion = args1.tail.head
                  withInlineBindings(owner,conv,runAwait(applyTerm, args.head, targs3.head.tpe, monadConversion, monadContext)(owner))
       case _ =>
         val cpsObj = runRoot(obj)(owner)
         if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"funSelect: cpsObj=${cpsObj}")
         cpsObj match
            case lt: AsyncLambdaCpsTree =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: AsyncLambdaCpsTree discovered, fun=$fun fun.tpe=${fun.tpe}")
               handleArgs1(applyTerm, fun, cpsObj.select(fun, fun.symbol, fun.tpe), args, tails)(owner)
            case cls: CallChainSubstCpsTree =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: CallChainSubstCpsTree discovered, fun=$fun fun.tpe=${fun.tpe}")
                  cpsCtx.log(s"funSelect: cls.shifted = ${cls.shifted.show}")
               sameSelect(cls.shifted, methodName, List.empty, args)(owner) match 
                  case None => 
                               if (cpsCtx.flags.debugLevel >= 15) then
                                  cpsCtx.log(s"not found name ${methodName} for ${cls.shifted.show}")
                               val cpsObj1 = cpsObj.select(fun,fun.symbol, fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails)(owner)
                  case Some(term) => 
                               if (cpsCtx.flags.debugLevel >= 15) then
                                  cpsCtx.log(s"found sameSelect: ${term.show} ")
                               val cpsObj1 = CpsTree.pure(owner, term, isChanged = true)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails, unpure=true)(owner)
            case _ =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: ! lambda || Subst, fun=$fun fun.tpe=${fun.tpe}")
               handleArgs1(applyTerm, fun, cpsObj.select(fun, fun.symbol, fun.tpe), args, tails)(owner)

  def withInlineBindings(owner: Symbol, origin: Inlined, tree:CpsTree):CpsTree =
        if (origin.bindings.isEmpty)
           tree
        else
           InlinedCpsTree(owner, origin, origin.bindings, tree)

  def handleFunIdent(applyTerm: Apply, fun:Term, args:List[Term], name: String, tails: List[Seq[ApplyArgRecord]])(owner: Symbol):CpsTree =
        handleArgs1(applyTerm, fun, CpsTree.pure(owner,fun), args, tails)(owner)

  def handleFunApply(applyTerm: Apply, fun:Term, args: List[Term],
                                      fun1: Term, args1: List[Term],
                                      tails: List[Seq[ApplyArgRecord]])(owner:Symbol):CpsTree =
        val paramsDescriptor = MethodParamsDescriptor(fun)
        val argsRecords = O.buildApplyArgsRecords(paramsDescriptor, args /*, cpsCtx*/)(owner)
        runApply(applyTerm, fun1, args1, argsRecords::tails)(owner)


  def handleFun(applyTerm: Apply, fun:Term, args:List[Term], tails: List[Seq[ApplyArgRecord]])(owner: Symbol):CpsTree =
       val cpsFun = runRoot(fun)(owner)
       handleArgs1(applyTerm, fun, cpsFun, args, tails)(owner)



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
   *@param cpsFun - cps-sed of representation of functions
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
                   )(owner: Symbol): CpsTree =  {
        if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s"handleArgs1, fun=${safeShow(fun)}")
            cpsCtx.log(s" cpsFun=${cpsFun}")
            cpsCtx.log(s" fun.symbol=${fun.symbol}")
            cpsCtx.log(s" fun.tpe=${fun.tpe}")
            cpsCtx.log(s" args=${args}")
            cpsCtx.log(s" tails=${tails}")
            cpsCtx.log(s" unpure=${unpure}")


        val paramsDescriptor = MethodParamsDescriptor(fun)
        val applyRecords = O.buildApplyArgsRecords(paramsDescriptor, args /*, cpsCtx*/)(owner)
        val argsProperties = ApplyArgsSummaryProperties.mergeSeqSeq(applyRecords :: tails)

        val existsAsyncArg = argsProperties.hasAsync
        val existsPrependArg = argsProperties.usePrepend
        val existsShiftedLambda = argsProperties.hasShiftedLambda
        val shouldBeChangedSync = argsProperties.shouldBeChangedSync
        val existsCpsDirect = argsProperties.cpsDirectArg.isDefined
        val callCpsDirect =  existsCpsDirect && !fun.symbol.hasAnnotation(cpsNotChangeSymbol)
                                                       //  && !fun.symbol.flags.is(Flags.Inline)
                                                         && !fun.symbol.name.contains("$")
        if (callCpsDirect && !fun.symbol.flags.is(Flags.Inline)) then
              val funSym = TransformUtil.extractCarriedFunSym(fun)
              if(!funSym.isDefinedInCurrentRun &&
                 !funSym.flags.is(Flags.Inline) &&
                 !funSym.hasAnnotation(cpsTransformedSymbol)  ) then
                 // situation, when external function is not transformed by cps-async.
                 //  note, that we can't determinate for function in the current compilation run, because
                 //  we can ba called from transparent inline function which run0s before compiler-plusing phase.
                   throw MacroError(s"Function ${fun.symbol} is cps-=direct function compiled without dotty-cps-async-compiler plugin", applyTerm.asExpr)
        if cpsCtx.flags.debugLevel >= 15 then
            cpsCtx.log(s" existsShiftedLambda=${existsShiftedLambda}")
            cpsCtx.log(s" existsAsyncArg=${existsAsyncArg}")
            cpsCtx.log(s" existsPrependArg=${existsPrependArg}")
            cpsCtx.log(s" shouldBeChangedSync=${shouldBeChangedSync}")
            cpsCtx.log(s" callCpsDirect=${callCpsDirect}")


        if (!existsAsyncArg && !existsShiftedLambda && !shouldBeChangedSync) {
           val tailArgss = tails.map(_.map(_.term).toList)
           cpsFun match
              case lt: AsyncLambdaCpsTree =>
                      if (callCpsDirect) then
                        throw MacroError("Lambda accepting CpsDirect is not supported yet",applyTerm.asExpr)
                      else
                        CpsTree.impure(owner,Select.unique(lt.rLambda,"apply").appliedToArgss(args::tailArgss), applyTerm.tpe)
              case _ =>
                      cpsFun.syncOrigin match
                         case Some(fun1) =>
                           if (!cpsFun.isChanged && !unpure)
                             if (callCpsDirect)
                               wrapCallIntoCpsDirect(applyTerm, argsProperties.cpsDirectArg.get, applyTerm)(owner)
                             else
                               CpsTree.pure(owner,applyTerm)
                           else
                             if (unpure)
                                val internalApply = fun1.appliedToArgss(args::tailArgss)
                                shiftedResultCpsTree(applyTerm, internalApply, argsProperties.cpsDirectArg)(owner)
                             else
                                val newApply = fun1.appliedToArgss(args::tailArgss)
                                optWrapCallIntoCpsDirect(applyTerm, argsProperties.cpsDirectArg, newApply, true)(owner)
                         case _ =>
                            cpsFun.monadMap(x => x.appliedToArgss(args::tailArgss), applyTerm.tpe)
        } else {
           val retval = cpsFun match {
               case lt: AsyncLambdaCpsTree  =>
                  buildApplyPrependArgsFlatMaps(cpsFun, fun, applyRecords, applyTerm, argsProperties, true, tails)(owner)        
               case _ =>
                  if (!cpsFun.isAsync) then
                     buildApplyPrependArgsFlatMaps(cpsFun, fun, applyRecords, applyTerm, argsProperties, unpure, tails)(owner)
                  else
                     //note,that if cpsFun is async, it should be caclulated before arguments
                     // (see https://github.com/rssh/dotty-cps-async/issues/67)
                     cpsFun match
                        case  sel: SelectTypeApplyCpsTree =>
                           //  if this is select[typeapply](obj) then at first cps-transform object
                           sel.nested.syncOrigin match
                              case Some(nested) =>
                                 buildApplyPrependArgsFlatMaps(cpsFun, fun, applyRecords, applyTerm, argsProperties, unpure, tails)(owner)
                              case None =>
                                 //  TODO:  optimize as with cpsFun branch above.
                                 sel.nested.monadFlatMap({ x =>
                                    val nFun = sel.apply(x)
                                    val nCpsFun = CpsTree.pure(owner,nFun,true)
                                    val cpsCall = buildApplyPrependArgsFlatMaps(nCpsFun, nFun, applyRecords, applyTerm, argsProperties, unpure, tails)(owner)
                                    cpsCall.transformed
                                 }, applyTerm.tpe.widen)
                        case _ =>
                           val tmpSym = Symbol.newVal(owner,"tmp_x",cpsFun.otpe.widen,Flags.EmptyFlags, Symbol.noSymbol)
                           val symRef = Ref(tmpSym)
                           val nCpsFun = CpsTree.pure(owner,symRef,true)
                           val cpsCallWithArgs = buildApplyPrependArgsFlatMaps(nCpsFun,fun,applyRecords, applyTerm, argsProperties, unpure, tails)(owner)      
                           cpsCallWithArgs.syncOrigin match
                              case Some(syncCall) =>
                                 cpsFun.monadMap({x => TransformUtil.changeSymsInTerm(Map(tmpSym -> x),syncCall,owner)},applyTerm.tpe.widen)
                              case None =>
                                 val callWithArgs = cpsCallWithArgs.transformed
                                 cpsFun.monadFlatMap({ x =>
                                    TransformUtil.changeSymsInTerm(Map(tmpSym -> x),callWithArgs,owner)
                                 }, applyTerm.tpe.widen)
           }
           if (cpsCtx.flags.debugLevel >= 15) then
               cpsCtx.log(s"handleArgs1: result = ${retval}")
           retval
        }
  }

  def optWrapCallIntoCpsDirect(originApplyTerm: Apply, optCpsDirectArg: Option[Term], call: Term, isChanged:Boolean)(owner: Symbol): CpsTree =
    optCpsDirectArg match
      case Some(cpsDirectArg) =>
        wrapCallIntoCpsDirect(originApplyTerm, cpsDirectArg, call)(owner)
      case None =>
        CpsTree.pure(owner, call, isChanged)

  def wrapCallIntoCpsDirect(origin: Term, cpsDirectArg: Term, applyCall: Term)(owner: Symbol): CpsTree = {
    cpsDirectArg match
      case Apply(TypeApply(byInclusionCn, List(tf, tg)), List(fctx, fgincl))
        if (byInclusionCn.symbol == Symbol.requiredMethod("cps.CpsDirect.byInclusion")) =>
          if (tf.tpe =:= tg.tpe) then
            val nCpsDirectArg = genCpsDirectDefaultConstructor(tf,fctx,cpsDirectArg)
            val tree = applyCall
            val adoptedTree = Apply.copy(origin)(
              TypeApply(Ref(adoptCpsedCallCompileTimeOnlySymbol),
                List(TypeTree.of[F], Inferred(origin.tpe.widen))),
                List(substituteCpsDirectArgRequired(tree, cpsDirectArg, nCpsDirectArg))
            )
            CpsTree.impure(owner, adoptedTree, origin.tpe.widen)
          else
            val tgctx = Symbol.requiredClass("cps.CpsTryMonadContext").typeRef.appliedTo(tg.tpe)
            val grt = tg.tpe.widen.appliedTo(origin.tpe.widen)
            val lambdaMethodType = MethodType(List("ctx"))(_ => List(tgctx), _ => grt)
            val lambda = Lambda(owner, lambdaMethodType, { (owner, params) =>
              val gctxArg = params.head.asInstanceOf[Term]
              val nCpsDirectArg = genCpsDirectDefaultConstructor(tg, gctxArg, cpsDirectArg)
              //  assume that tree is generated with new owner
              val tree = applyCall
              // TODO:mb move substirute args to the later step
              val adoptedTree = Apply.copy(origin)(
                TypeApply(Ref(adoptCpsedCallCompileTimeOnlySymbol),
                  List(tg, Inferred(origin.tpe.widen))),
                  List(substituteCpsDirectArgRequired(tree, cpsDirectArg, nCpsDirectArg))
              )
              adoptedTree.changeOwner(owner)
            })
            val r = Apply.copy(origin)(
              Apply(
                TypeApply(Select.unique(fgincl, "apply"), List(Inferred(origin.tpe.widen))),
                List(fctx)
              ),
              List(lambda)
            )
            CpsTree.impure(owner, r, origin.tpe.widen)
      case _ =>
        val tree = applyCall
        val adoptedTree = Apply.copy(origin)(
          TypeApply(Ref(adoptCpsedCallCompileTimeOnlySymbol),
            List(Inferred(TypeRepr.of[F]), Inferred(origin.tpe.widen))),
          List(tree)
        )
        CpsTree.impure(owner, adoptedTree, origin.tpe.widen)
  }

  def genCpsDirectDefaultConstructor(tf: TypeTree, fctx: Term, posTerm:Term): Term =
    val cpsDirectApply = Symbol.requiredMethod("cps.CpsDirect.apply")
    Apply.copy(posTerm)(
      TypeApply(Ref(cpsDirectApply), List(tf)),
      List(fctx)
    )
    //val cpsDirectType = Symbol.requiredClass("cps.CpsDirect").typeRef.appliedTo(tf.tpe)
    //val cpsDirectConstructor = Select.unique(New(Inferred(cpsDirectType)),"<init>")
    //Apply.copy(posTerm)(
    //  TypeApply(cpsDirectConstructor, List(tf)),
    //  List(fctx)
    //)

  def substituteCpsDirectArgRequired(term: Term, originCpsDirectArg: Term, nCpsDirectArg: Term): Term =
    substituteCpsDirectArg(term,originCpsDirectArg,nCpsDirectArg).getOrElse(
      throw MacroError(s"Can't find cpsDirectArg context parameter ${originCpsDirectArg} in ${term}",term.asExpr)
    )

  def substituteCpsDirectArg(term: Term, originCPsDirectArg: Term, nCpsDirectArg:Term): Option[Term] = {
    term match
      case ap@Apply(f, args) =>
        substituteCpsDirectArgApply(ap, originCPsDirectArg, nCpsDirectArg)
      case TypeApply(f, targs) =>
        substituteCpsDirectArg(f, originCPsDirectArg, nCpsDirectArg).map(_ => TypeApply.copy(term)(f, targs))
      case _ => None
  }

  def substituteCpsDirectArgApply(applyTerm:Apply, originCPsDirectArg: Term, nCpsDirectArg:Term): Option[Apply] = {
    var found = false
    val newArgs = applyTerm.args.map{ arg =>
      if (isCpsDirectByInclusionCall(arg, Some(originCPsDirectArg))) {
        found = true
        nCpsDirectArg
      } else {
        arg
      }
    }
    if (found) {
      Some(Apply.copy(applyTerm)(applyTerm.fun, newArgs))
    } else {
      substituteCpsDirectArg(applyTerm.fun, originCPsDirectArg, nCpsDirectArg).map(newFun =>
        Apply.copy(applyTerm)(newFun, applyTerm.args)
      )
    }
  }

  def isCpsDirectByInclusionCall(arg: Term, originCPsDirectArg: Option[Term]): Boolean = {
    // in future - check that originCpsDirectArg is the same as arg (but can be copied and with other owner)
    //  for now - assume that we have obly one CpsDirect[F] context parameter
    arg match
      case Apply(TypeApply(byInclusionCn, List(tf, tg)), List(fctx, fgincl))
        if (byInclusionCn.symbol == Symbol.requiredMethod("cps.CpsDirect.byInclusion")) =>
          true
      case _ => false
  }
 
  def buildApplyPrependArgsFlatMaps(cpsFun: CpsTree, fun: Term,
                 argsRecords: Seq[ApplyArgRecord],
                 applyTerm: Apply,
                 argsProperties: ApplyArgsSummaryProperties,
                 unpure: Boolean,
                 tails: List[Seq[ApplyArgRecord]]
                 )(owner: Symbol): CpsTree =  {

      var runFold = true           
      val lastCpsTree: CpsTree = if (!argsProperties.usePrepend && cpsFun.isSync) {
                  runFold = false
                  if (!argsProperties.hasShiftedLambda && !cpsFun.isChanged && 
                      !unpure && !argsProperties.shouldBeChangedSync)
                     optWrapCallIntoCpsDirect(applyTerm, argsProperties.cpsDirectArg, applyTerm, false)(owner)
                  else
                     buildApply(cpsFun, fun, argsRecords, applyTerm, argsProperties, unpure, tails)(owner)
               } else {
                  buildApply(cpsFun, fun, argsRecords, applyTerm, argsProperties, unpure, tails)(owner)
               } 
               
      val retval = if (runFold) {
            (argsRecords::tails).foldRight(lastCpsTree){(pa,sa) =>
               pa.foldRight(sa){ (p,s) =>
                  if (p.usePrepend(argsProperties.hasAsync))
                     p.append(s)
                  else
                     s
               }
            }
         } else lastCpsTree

      retval
  }


  def findAsyncShiftTerm(e:Term):(ImplicitSearchResult, TypeRepr) =
    val tpe = e.tpe.widen
    val asyncShift = TypeIdent(Symbol.classSymbol("cps.AsyncShift")).tpe
    val asyncShiftType = asyncShift.appliedTo(tpe)
    (Implicits.search(asyncShiftType), asyncShiftType)




  def shiftedApplyCps(
                      funCpsTree: CpsTree, 
                      argRecords: Seq[ApplyArgRecord],
                      argTails: List[Seq[ApplyArgRecord]],
                      applyTerm: Apply,
                      argsSummaryProperties: ApplyArgsSummaryProperties)(owner: Symbol): CpsTree =

    def condTypeApply(sel:Term, targs: List[TypeTree]):Term =
      if (targs.isEmpty) sel else TypeApply(sel,targs)

    val withAsync = argsSummaryProperties.hasAsync

    def applyPartialShift(head: PartialShiftedApply): CpsTree = {
      val callDelayed = head.withTailArgs(argTails, withAsync)
      head.shiftType match {
        case ApplicationShiftType.CPS_ONLY =>
          val call = callDelayed('{ ??? }.asTerm)
          shiftedResultCpsTree(applyTerm, call, argsSummaryProperties.cpsDirectArg)(owner)
        case ApplicationShiftType.CPS_AWAIT =>
          cpsCtx.runtimeAwait match
            case Some(runtimeAwait) =>
              val call = callDelayed(runtimeAwait.asTerm)
              shiftedResultCpsTree(applyTerm, call, argsSummaryProperties.cpsDirectArg)(owner)
            case None =>
              cpsCtx.runtimeAwaitProvider match
                case Some(runtimeAwaitProvider) =>
                  val raLambdaMt = MethodType(List("runtimeAwait"))(
                    _ => List(TypeRepr.of[CpsRuntimeAwait[F]]),
                    _ => TypeRepr.of[F].appliedTo(applyTerm.tpe.widen)
                  )
                  val raLambda = argsSummaryProperties.cpsDirectArg match
                    case Some(cpsDirectArg) =>
                      //
                      Lambda(owner, raLambdaMt, { (owner, params) =>
                        val runtimeAwait = params.head.asInstanceOf[Term]
                        // here is how constraint about owner can be violated.
                        //  TODO:  add owner argument to call delayed,
                        val funCall = callDelayed(runtimeAwait).changeOwner(owner)
                        val cpsTree = wrapCallIntoCpsDirect(applyTerm,cpsDirectArg, funCall)(owner)
                        cpsTree.transformed
                      })
                    case None =>
                      Lambda(owner, raLambdaMt, { (owner, params) =>
                          val runtimeAwait = params.head.asInstanceOf[Term]
                          val plainCall = callDelayed(runtimeAwait).changeOwner(owner)
                          Apply.copy(applyTerm)(
                            TypeApply(Select.unique(cpsCtx.monad.asTerm, "pure"),List(Inferred(applyTerm.tpe.widen))),
                            List(plainCall)
                          )
                      })
                  val call = Apply.copy(applyTerm)(
                              Apply(
                                TypeApply(Select.unique(runtimeAwaitProvider.asTerm, "withRuntimeAwait"),
                                          List(Inferred(applyTerm.tpe.widen))),
                                List(raLambda)
                             ),
                             List(cpsCtx.monadContext.asTerm)
                  )
                  CpsTree.impure(owner, call, applyTerm.tpe.widen)
                case None =>
                  throw MacroError("Can't find runtime await provider", applyTerm.asExpr)
        case  ApplicationShiftType.CPS_DEFERR_TO_PLUGIN =>
          //val call = callDelayed('{ ??? }.asTerm)
          val call = applyTerm
          val retval =
            Apply(
              Apply(
                TypeApply(
                  Ref(Symbol.requiredMethod("cps.plugin.scaffolding.deferredAsync")),
                  List(Inferred(call.tpe.widen), Inferred(TypeRepr.of[F]), Inferred(TypeRepr.of[CC]))
                ),
                List(call)
              ),
              List(cpsCtx.monadContext.asTerm)
          )
          CpsTree.impure(owner, retval, applyTerm.tpe.widen)

      }


    }


    
    funCpsTree.syncOrigin match
      case Some(origin) =>
        val partialHead = shiftedApplyTerm(origin, argRecords, argsSummaryProperties)
        applyPartialShift(partialHead)
      case None =>
        (funCpsTree: @nowarn("msg=Unreachable case .*")) match
           case _ : PureCpsTree  |  EmptyCpsTree =>
              // impossible
              val originTerm = funCpsTree.syncOrigin.get
              val head = shiftedApplyTerm(originTerm, argRecords, argsSummaryProperties)
              applyPartialShift(head)
           case SelectTypeApplyCpsTree(optOrigin,nested,targs,selects,otpe, changed) =>
              if (selects.isEmpty) then
                 if (targs.isEmpty) then
                   shiftedApplyCps(nested,argRecords,argTails,applyTerm, argsSummaryProperties)(owner)
                 else
                   // this can be only generice async-lambda
                   //  which is impossible to create in scala syntax
                   throw MacroError(s"can't shift async head: ${funCpsTree}", posExprs(applyTerm))
              else
                 val current = selects.head
                 val prevSelects = selects.tail
                 val prev = SelectTypeApplyCpsTree.create(optOrigin,nested,targs,prevSelects,current.prevTpe, changed)
                 prev.monadFlatMap({x =>
                       val funToShift = condTypeApply(Select(x,current.symbol),current.targs)
                       val head = shiftedApplyTerm(funToShift,argRecords,argsSummaryProperties)
                       //head.withTailArgs(argTails, withAsync)
                       applyPartialShift(head).transformed
                 }, applyTerm.tpe)
           case lt@AsyncLambdaCpsTree(owner,originLambda,params,body,otpe) =>
              val head = shiftedApplyTerm(lt.rLambda, argRecords, argsSummaryProperties)
              applyPartialShift(head)
           case x: AsyncCpsTree =>
              // this can be only function, so, try to call apply method
              x.monadFlatMap({fun =>
                  val head = shiftedApplyTerm(Select.unique(fun,"apply"), argRecords, argsSummaryProperties)
                  applyPartialShift(head).transformed
              }, applyTerm.tpe)
           case BlockCpsTree(owner,stats, last) =>
                  BlockCpsTree(owner,stats,shiftedApplyCps(last,argRecords,
                               argTails, applyTerm, argsSummaryProperties)(owner))
           case InlinedCpsTree(owner,origin, bindings, nested) =>
                  InlinedCpsTree(owner,origin, bindings, shiftedApplyCps(nested, argRecords,
                                         argTails, applyTerm, argsSummaryProperties)(owner))
           case ValCpsTree(owner,valDef, rightPart, nested, canBeLambda) =>
                  ValCpsTree(owner,valDef, rightPart, shiftedApplyCps(nested, argRecords,
                                        argTails, applyTerm, argsSummaryProperties)(owner), canBeLambda)
           case AppendCpsTree(frs, snd) =>
                  AppendCpsTree(frs,
                      shiftedApplyCps(snd,argRecords,argTails,applyTerm, argsSummaryProperties)(owner))
           case CallChainSubstCpsTree(owner, origin, shifted, otpe) =>
                  val head = shiftedApplyTerm(Select.unique(shifted,"apply"), argRecords, argsSummaryProperties)
                  applyPartialShift(head)
                  //shiftedResultCpsTree(applyTerm, head.withTailArgs(argTails, withAsync), argsSummaryProperties.cpsDirectArg)(owner)
           case null => // impossible, but let's check
                 throw MacroError(s"Unsupported fun: CpsTree:  $funCpsTree", applyTerm.asExpr)



  def shiftedApplyTerm(funTerm: Term, argRecords: Seq[ApplyArgRecord], argsSummaryProperties: ApplyArgsSummaryProperties): PartialShiftedApply =

    val monad = cpsCtx.monad.asTerm

    def shiftArgs(): List[Term] =
      argRecords.map(_.shift().identArg(argsSummaryProperties.hasAsync)).toList

    def argsWithRuntimeAwait(runtimeAwait: Term): List[Term] =
      argRecords.map(_.withRuntimeAwait(runtimeAwait).identArg(argsSummaryProperties.hasAsync)).toList

    def applyCpsOnlyShift(makeApply: List[Term] => Term): PartialShiftedApply =
      val newArgs = shiftArgs()
      val newApply = makeApply(newArgs)
      PartialShiftedApply(ApplicationShiftType.CPS_ONLY, _ => newApply)

    def  applyCpsAwaitShift(): PartialShiftedApply =
      // TODO:  move up after refactoring
      val delayedShift = (runtimeAwait: Term) => {
        val newArgs = argsWithRuntimeAwait(runtimeAwait)
        val newApply = Apply(funTerm, newArgs)
        newApply
      }
      PartialShiftedApply(ApplicationShiftType.CPS_AWAIT, delayedShift)

    def applyCpsDeferredShift(): PartialShiftedApply =
      PartialShiftedApply(ApplicationShiftType.CPS_DEFERR_TO_PLUGIN,
        (unused) => Apply(funTerm, argRecords.map(_.term).toList)  // TODO: we can eliminate this if we have origin term
      )

    def checkInplaceAsyncMethodCandidate(methodSym: Symbol, qual: Term, targs: List[TypeTree]): Either[MessageWithPos,Boolean] =
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

    def findInplaceAsyncMethodCall(qual:Term, shiftedName: String,
                                   targs: List[TypeTree], pos: Position): Either[List[MessageWithPos],PartialShiftedApply] =
         //val qual = x.qualifier

         def withTargs(t:Term):Term =
           if (targs.isEmpty)
             t
           else
             TypeApply(t, targs)

         qual.tpe.typeSymbol.methodMember(shiftedName) match
           case Nil =>
             Left(List())
           case m::Nil =>
             checkInplaceAsyncMethodCandidate(m, qual, targs) match
               case Left(error) => Left(List(error))
               case Right(useExtraArgs) =>
                 val newArgs = shiftArgs()
                 val newApply = if (useExtraArgs) then {
                        Apply(TypeApply(Select.unique(qual,shiftedName), TypeTree.of[F]::targs), monad::newArgs)
                     } else {
                        Apply(withTargs(Select.unique(qual,shiftedName)), newArgs)
                     }
                 Right(PartialShiftedApply(ApplicationShiftType.CPS_ONLY, _ => newApply))
           case overloaded =>
               var errors: List[MessageWithPos] = List.empty
               var foundUseExtra: List[Symbol] = List.empty
               var foundDirect: List[Symbol] = List.empty
               var c = overloaded
               while(!c.isEmpty) {
                 val sym = c.head
                 c = c.tail
                 checkInplaceAsyncMethodCandidate(sym, qual, targs) match
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
                      Right(applyCpsOnlyShift(
                         args => Select.overloaded(qual,shiftedName,TypeTree.of[F].tpe::targs.map(_.tpe), monad::args),
                      ))
               else
                   if (foundUseExtra.isEmpty) then
                     Right(applyCpsOnlyShift(
                        args => Select.overloaded(qual,shiftedName,targs.map(_.tpe), args)
                     ))
                   else
                      // found both variants, can't choose ?
                      Left(List(
                        MessageWithPos(s"More than one candidate for overloaded variant for method $shiftedName, qual=${qual.show}",pos)
                      ))


    def shiftSelectTypeApplyApply(x: Select, targs: List[TypeTree]): PartialShiftedApply =
       // Looks like this code is obsolete when we have more general substitution.
       //  (still no, because scala.collection.WithFilter not give access to the underlying collection,
       //   so it is impossible to write async oprtations on it)
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
                 val newArgs =  shiftArgs()
                 // TODO:   set pos.  mb pass origin typeApply for this.
                 val newTerm = TypeApply(newSelect, TypeTree.of[F]::targs).appliedTo(monad).appliedToArgs(newArgs)
                 PartialShiftedApply(ApplicationShiftType.CPS_ONLY, _ => newTerm)
              case failure: ImplicitSearchFailure =>
                 if (cpsCtx.runtimeAwait.isDefined || cpsCtx.runtimeAwaitProvider.isDefined) then
                    applyCpsAwaitShift()
                 else
                    throw MacroError(s"Can't resolve [${askedShiftedType.show}] when parsing withFilter ",posExpr(col))
         case _ =>
            shiftSelectTypeApplyApplyClear(x.qualifier, x, targs)



    def shiftSelectTypeApplyApplyClear(qual:Term, x: Ref, targs:List[TypeTree]): PartialShiftedApply =

       //val qual = x.qualifier
       val xName = x match
         case Ident(name) => name
         case Select(_,name) => name


       def traceFunNotFound(msg:String, errors:List[MessageWithPos]): Unit =
          if (!errors.isEmpty) then
            report.warning(msg)
            errors.foreach(e =>
              report.warning(e.message)
            )

       val shiftedName = xName + "_async"
       findInplaceAsyncMethodCall(qual, shiftedName,  targs, x.pos) match
         case Right(t) => t
         case Left(funErrors) =>
           val funErrors0 = funErrors
           val shiftedName1 = xName + "Async"
           findInplaceAsyncMethodCall(qual, shiftedName1,  targs, x.pos) match
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
                   shiftSymbol.methodMember(xName) match
                     case Nil =>
                        if (cpsCtx.runtimeAwait.isDefined || cpsCtx.runtimeAwaitProvider.isDefined) {
                            applyCpsAwaitShift()
                        } else {
                            throw MacroError(s"Method (${xName}) is not defined in [${shiftType.show}], qual=${qual} ", posExpr(x))
                        }
                     case m::Nil =>
                        applyCpsOnlyShift{ args =>
                           val newSelect = Select.unique(success2.tree, xName)
                           TypeApply(newSelect, TypeTree.of[F]::targs).appliedTo(qual,monad).appliedToArgs(args)
                        }
                     case other =>
                        applyCpsOnlyShift{ args =>
                           // TODO: other args in typebounds [?]
                           val shiftedArgTypes = args.map(_.tpe)
                           val expectedType = TransformUtil.createFunctionType(using qctx)(shiftedArgTypes, TypeBounds.empty)
                           val shiftedCaller = Select.overloaded(success2.tree, xName, (TypeTree.of[F]::targs).map(_.tpe), List(qual,monad), expectedType)
                           Apply(shiftedCaller, shiftArgs())
                        }
                 case failure2: ImplicitSearchFailure =>
                   if (x.symbol.isDefinedInCurrentRun &&
                       x.symbol.hasAnnotation(Symbol.requiredClass("cps.plugin.annotation.makeCPS")) ) then
                       // because cps method will be generated after typer, we in transparent macro call which in typer.
                       applyCpsDeferredShift()
                       //throw MacroError(s"Can't shift method ${xName} marked with @makeCps (not implemented yet)", posExpr(x))
                   else  if (cpsCtx.runtimeAwait.isDefined || cpsCtx.runtimeAwaitProvider.isDefined) then
                     applyCpsAwaitShift()
                   else
                     traceFunNotFound(s"failed candidates for ${qual.show} ${shiftedName}",funErrors)
                     if cpsCtx.flags.debugLevel >= 15 then
                        for((a,i) <- argRecords.zipWithIndex) {
                           cpsCtx.log(s"arg($i)=${a.term.show}")
                        }
                     throw MacroError(s"Can't find AsyncShift (${failure2.explanation}) or async functions) for qual=${qual} name = ${xName}, shiftedName=${shiftedName}, askedShiftedType=${askedShiftedType.show}",posExpr(x))

    def shiftIdentTypeApply(x:Ident, targs: List[TypeTree]): PartialShiftedApply = {
       val owner = x.symbol.maybeOwner
       if (owner.isType  && owner.name.endsWith("$")) {
         // i.e. we have ident which is a method of imported object.
         val qual = Ref.term(owner.companionModule.termRef)
         shiftSelectTypeApplyApplyClear(qual,x,targs)
       } else {
         throw MacroError(s"Can't determinate qual for ${x.show} during search of AsyncShift",posExpr(x))
       }
    }

    funTerm match
       case TypeApply(s@Select(qual,name),targs) =>
                  shiftSelectTypeApplyApply(s, targs)
       case TypeApply(id@Ident(_),targs) =>
                  shiftIdentTypeApply(id, targs)
       case s@Select(qual,name) =>
                    shiftSelectTypeApplyApply(s, Nil)
       case id@Ident(name) =>
                    shiftIdentTypeApply(id, Nil)             
       //case TypeApply(x, targs) =>  // now scala hvw no multiple type params
       //           Apply(TypeApply(shiftCaller(x),targs),args)
       case Lambda(params, body) =>
            if (cpsCtx.runtimeAwait.isDefined || cpsCtx.runtimeAwaitProvider.isDefined) then
               applyCpsAwaitShift()
            else
               throw MacroError(s"Can't shift lambda ${funTerm.show} without runtime await",posExprs(funTerm))
       case Block(statements, last) =>
                  //  it's not eta-expandedn. (never happens?)
                  // TODO: eta-expand to lambda themself ?
                  val pa = shiftedApplyTerm(last, argRecords, argsSummaryProperties)
                  pa.copy(shiftedDelayed = x => Block(statements,pa.shiftedDelayed(x)))
       case _ =>
            if (cpsCtx.runtimeAwait.isDefined || cpsCtx.runtimeAwaitProvider.isDefined) then
               applyCpsAwaitShift()
            else
               report.warning(s"""
                     Need to shift $funTerm, tpe.widen=${funTerm.tpe.widen}
                     argRecords=${argRecords}
               """, posExprs(funTerm))
               throw MacroError(s"Can't shift caller ${funTerm.show} (tree ${funTerm})",posExprs(funTerm))

  end shiftedApplyTerm


  def buildApply(cpsFun: CpsTree, fun: Term,
                 argRecords: Seq[ApplyArgRecord],
                 applyTerm: Apply,
                 argsProperties: ApplyArgsSummaryProperties,
                 inShiftedCallChain: Boolean,
                 tails: List[Seq[ApplyArgRecord]]
                 )(owner: Symbol): CpsTree =
        if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"buildApply: fun=${safeShow(fun)}")
            cpsCtx.log(s"buildApply: cpsFun.isSync=${cpsFun.isSync}, withShiftedLambda=${argsProperties.hasShiftedLambda}, inShiftedCallChain=${inShiftedCallChain}")
        val origFun = fun
        val withAsync = argsProperties.hasAsync
        val applyTpe = applyTerm.tpe
        if (argsProperties.hasShiftedLambda)
          buildShiftedApply(cpsFun, fun, argRecords, argsProperties, tails, applyTerm)(owner)
        else
          //TODO("hanlde when argsProperties.hasCpsDirectArg")
          val args = argRecords.map(_.identArg(withAsync)).toList
          val tailArgss = tails.map(_.map(_.identArg(withAsync)).toList)
          val argss = args::tailArgss
          val retval = cpsFun match
             case lt:AsyncLambdaCpsTree =>
                    //TODO: select.uniqe("apply") ?   weite test-case
                    CpsTree.impure(owner,lt.rLambda.appliedToArgss(argss), applyTpe)
             case cs:CallChainSubstCpsTree =>
                    if (cpsCtx.flags.debugLevel >= 15) {
                       cpsCtx.log(s"buildApply: cs:CallChainSubstCpsTree")
                    }
                    shiftedResultCpsTree(applyTerm, cs.shifted.appliedToArgss(argss), argsProperties.cpsDirectArg)(owner)
             case _ =>
                    cpsFun.syncOrigin match
                       case Some(fun) =>
                            val applied = try {
                              fun.appliedToArgss(argss)
                            } catch {
                              case ex: Throwable =>
                                cpsCtx.log(s"buildApply: fun=${safeShow(fun)}")
                                throw ex
                            }
                            if (inShiftedCallChain)
                               shiftedResultCpsTree(applyTerm, applied, argsProperties.cpsDirectArg)(owner)
                            else
                               optWrapCallIntoCpsDirect(applyTerm, argsProperties.cpsDirectArg, applied, true)(owner)
                       case None =>
                          if (inShiftedCallChain)
                             val shifted = cpsFun.transformed
                             shiftedResultCpsTree(applyTerm, shifted.appliedToArgss(argss), argsProperties.cpsDirectArg)(owner)
                          else
                            argsProperties.cpsDirectArg match
                              case Some(cpsDirectArg) =>
                                cpsFun.monadFlatMap({ x =>
                                  wrapCallIntoCpsDirect(applyTerm, cpsDirectArg, x.appliedToArgss(argss))(owner).transformed }, applyTpe)
                              case None =>
                                cpsFun.monadMap({ x => x.appliedToArgss(argss) }, applyTpe)
          if (cpsCtx.flags.debugLevel >= 15) {
            cpsCtx.log(s"buildApply: retval = $retval")
          }
          retval

  def shiftedResultCpsTree(origin: Term, shifted: Term, optCpsDirectArg: Option[Term])(owner: Symbol): CpsTree = {
            if (shifted.tpe.widen =:= origin.tpe.widen) then
                // when shifted result type is the same as result type.
                // can be during cpsTrre.await
                optCpsDirectArg match
                  case Some(cpsDirectArg) =>
                    wrapCallIntoCpsDirect(origin, cpsDirectArg, shifted)(owner)
                  case None =>
                    CpsTree.pure(owner, shifted, isChanged = true)
            else if (shifted.tpe.isFunctionType) then
                // TODO: extract argument types. now - one experiment
                shifted.tpe match
                  case AppliedType(f, List(a, AppliedType(m, b))) if f <:< TypeRepr.of[Function1] =>
                    val sym = Symbol.newVal(owner, "shiftedArg", a.widen, Flags.EmptyFlags, Symbol.noSymbol)
                    AsyncLambdaCpsTree(owner, origin, List(ValDef(sym, None)),
                      // will be changed.   (TODO: changer asyncLamfda signature ?[owner=>body instead body.])
                       CpsTree.impure(owner, Apply.copy(origin)(Select.unique(shifted, "apply"), List(Ref(sym))), b.head),
                      origin.tpe)
                  case _ =>
                    throw MacroError("Async function with arity != 1 is not supported yet", posExprs(shifted, origin))
            else if (shifted.tpe <:< TypeRepr.of[cps.runtime.CallChainAsyncShiftSubst[F, ?, ?]]) then
                    CallChainSubstCpsTree(owner, origin, shifted, origin.tpe)
            else
              optCpsDirectArg match
                  case Some(cpsDirectArg) =>
                    wrapCallIntoCpsDirect(origin, cpsDirectArg, shifted)(owner)
                  case None =>
                    CpsTree.impure(owner, shifted, origin.tpe)
  }


  def buildShiftedApply(
                                 cpsFun: CpsTree,
                                 fun: Term,
                                 argRecords: Seq[ApplyArgRecord],
                                 argsProperties: ApplyArgsSummaryProperties,
                                 tails: List[Seq[ApplyArgRecord]],
                                 applyTerm: Apply)(owner: Symbol): CpsTree = {
          if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"buildShiftedApply::fun=${fun}")
            cpsCtx.log(s"buildShiftedApply::argRecords=${argRecords}")
            cpsCtx.log(s"buildShiftedApply::tails=${tails}")
          shiftedApplyCps(cpsFun, argRecords, tails, applyTerm, argsProperties)(owner)
  }




  /**
   * Shift the functional typr tpe. annotated ny makecps.
   * If the function type does not contains hight-order function which can be shifted - report error.
   * (we call shift only for methods, annotated with makeCps)
   * Note, that is is not the type of the shifted methods (because it includes yet one type parameter F[_]),
   *  it is just the type which used to make typer happy with the shifted arguments.  This type will be throwed out on the later stage.
   * @param tpe
   * @param foundShiftedParams
   * @param pos
   * @return
   */
  def shiftTypeForCpsGeneratedMethod(tpe: TypeRepr, foundShiftedParams: Boolean, xPos: Term): TypeRepr = {

    val retval = tpe match
      case PolyType(names, bounds, res) =>
        val nBounds = (np: PolyType) => bounds.map(b => typeBoundSubst(b, List(tpe.typeSymbol), List(np)))
        val nRes = (np: PolyType) => shiftTypeForCpsGeneratedMethod(res.substituteTypes(List(tpe.typeSymbol), List(np)), foundShiftedParams, xPos)
        PolyType(names)(nBounds, nRes)
      case MethodType(names, params, res) =>
        val haveShiftedParams = foundShiftedParams || params.exists(p => isShiftedParam(p))
        //val shiftedParams = (np: MethodType) => params.map(x => shiftParamType(x.substituteTypes(List(tpe.typeSymbol), List(np)), xPos))
        //val shiftedRes = (np: MethodType) => shiftTypeForCpsGeneratedMethod(res.substituteTypes(List(tpe.typeSymbol), List(np)), haveShiftedParams, xPos)
        //MethodType(names)(shiftedParams, shiftedRes)
        //  will
        val shiftedParams = params.map(x => shiftParamType(x, xPos))
        val shiftedRes = shiftTypeForCpsGeneratedMethod(res, haveShiftedParams, xPos)
        defn.FunctionClass(params.length,false,false).typeRef.appliedTo(shiftedParams.appended(shiftedRes))
      case AppliedType(t, targs) =>
        val len = targs.length
        if (tpe.isFunctionType || tpe.isContextFunctionType) then
          var shiftedParamFound = foundShiftedParams
          val nTargs = targs.zipWithIndex.map { (p, i) =>
            if (i == len - 1) then
              shiftTypeForCpsGeneratedMethod(p, shiftedParamFound, xPos)
            else
              if (!shiftedParamFound) then
                shiftedParamFound = isShiftedParam(p)
                if (shiftedParamFound) then
                  shiftParamType(p, xPos)
                else
                  p
              else
                shiftParamType(p, xPos)
          }
          if (tpe.isFunctionType) then
            t.appliedTo(nTargs)
          else
            defn.FunctionClass(len,false,false).typeRef.appliedTo(nTargs)
        else
          if (!foundShiftedParams) then
            report.errorAndAbort(s"@makeCps annotated method ${xPos} have no shifted args ", xPos.pos)
          else
            TypeRepr.of[F].appliedTo(tpe.widen)
      case _ =>
        TypeRepr.of[F].appliedTo(tpe.widen)

    retval
  }

  def typeBoundSubst(tpb: TypeBounds, from: List[Symbol], to: List[TypeRepr]): TypeBounds = {
    TypeBounds(tpb.low.substituteTypes(from,to), tpb.hi.substituteTypes(from,to))
  }

  def isShiftedParam(tp: TypeRepr): Boolean = {
    tp match
      case AppliedType(t, targs) if (t.isFunctionType || t.isContextFunctionType) => true
      case ByNameType(tp1) => true
      case _ => false
  }


  def shiftParamType(tp: TypeRepr, xPos: Term): TypeRepr = {
    val retval = tp match
      case AppliedType(t, targs) =>
        if (tp.isFunctionType || tp.isContextFunctionType) then
          cpsTransformedType(tp)
        else
          tp
      case ByNameType(tp1) =>
        defn.FunctionClass(1,false,false).typeRef.appliedTo(List(tp1))
      case _ => tp
    retval
  }


  def cpsTransformedType(tpe: TypeRepr): TypeRepr = {
    val retval = tpe match
      case AppliedType(funCn, params) =>
        if (tpe.isFunctionType || tpe.isContextFunctionType) then
          val paramsLen = params.length
          val nParams = params.zipWithIndex.map { (p, i) =>
            if (i == paramsLen - 1) then
              cpsTransformedType(p)
            else
              p
          }
          AppliedType(funCn, nParams)
        else
          TypeRepr.of[F].appliedTo(tpe.widen)
      case mt@MethodType(paramNames, paramTypes, resType) =>
        MethodType(paramNames)(
          newMethod => paramTypes.map(_.substituteTypes(List(mt.typeSymbol), List(newMethod))),
          newMethod => cpsTransformedType(resType.substituteTypes(List(mt.typeSymbol), List(newMethod)))
        )
      case pt@PolyType(paramNames, paramTypes, resType) =>
        PolyType(paramNames)(
          npt => paramTypes.map(typeBoundSubst(_, List(pt.typeSymbol), List(npt))),
          npt => cpsTransformedType(resType.substituteTypes(List(pt.typeSymbol), List(npt)))
        )
      case _ =>
        TypeRepr.of[F].appliedTo(tpe.widen)
    retval
  }



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
                             )(qctx.reflect.Symbol.spliceOwner)
              val exprResult = treeResult.toResult[T]
              exprResult

     }
     (new Bridge).bridge()
  }


