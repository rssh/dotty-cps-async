package cps.forest

import scala.quoted._

import cps.{TransformationContextMarker=>TCM, _}
import cps.misc._

trait ApplyTreeTransform[F[_],CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.reflect._


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
                               handleArgs1(applyTerm, term, CpsTree.pure(term, isChanged=true), args, tails, unpure=true)
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
         if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"funSelect: cpsObj=${cpsObj}")
         cpsObj match
            case lt: AsyncLambdaCpsTree =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: AsyncLambdaCpsTree discovered, fun=$fun fun.tpe=${fun.tpe}")
               handleArgs1(applyTerm, fun, cpsObj.select(fun.symbol, fun.tpe), args, tails)
            case cls: CallChainSubstCpsTree =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: CallChainSubstCpsTree discovered, fun=$fun fun.tpe=${fun.tpe}")
               sameSelect(cls.shifted, methodName, List.empty, args) match 
                  case None => val cpsObj1 = cpsObj.monadMap(x => Select(x,fun.symbol), fun.tpe)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails)
                  case Some(term) => val cpsObj1 = CpsTree.pure(term, isChanged = true)
                               handleArgs1(applyTerm, fun, cpsObj1, args, tails, unpure=true)
            case _ =>
               if (cpsCtx.flags.debugLevel >= 15)
                  cpsCtx.log(s"funSelect: ! lambda || Subst, fun=$fun fun.tpe=${fun.tpe}")
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
        val argsRecords = O.buildApplyArgsRecords(paramsDescriptor, args, cpsCtx)
        runApply(applyTerm, fun1, args1, argsRecords::tails)


  def handleFun(applyTerm: Term, fun:Term, args:List[Term], tails: List[Seq[ApplyArgRecord]]):CpsTree =
       val cpsFun = runRoot(fun, TCM.ApplyFun)
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

        val applyRecords = O.buildApplyArgsRecords(paramsDescriptor, args, cpsCtx)
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
                                    if (!existsShiftedLambda && !cpsFun.isChanged && !unpure)
                                       CpsTree.pure(applyTerm)
                                    else
                                       buildApply(cpsFun, fun, applyRecords, applyTerm, existsAsyncArg, existsShiftedLambda, unpure, tails)
                                 } else {
                                    buildApply(cpsFun, fun, applyRecords, applyTerm, existsAsyncArg, existsShiftedLambda, unpure, tails)
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

  // TODO: remove
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
    Implicits.search(tpTree)

  def findObjectAsyncShiftTerm(e:Term):ImplicitSearchResult =
    val tpe = e.tpe.widen
    val objAsyncShift = TypeIdent(Symbol.classSymbol("cps.ObjectAsyncShift")).tpe
    val tpTree = objAsyncShift.appliedTo(tpe)
    //val tpTree = Type.of[ObjectAsyncShift].appliedTo(tpe).simplified
    if cpsCtx.flags.debugLevel >= 15 then
      cpsCtx.log(s"searchImplicits: tpTree=$tpTree")
      cpsCtx.log(s"tpe=$tpe")
      cpsCtx.log(s"Type.of[ObjectAsyncShift]=${TypeRepr.of[ObjectAsyncShift]}")
    Implicits.search(tpTree)


  def shiftedApply(term: Term, originArgs:List[Term], shiftedArgs: List[Term], shiftedIndexes:Set[Int]): Term =

    val monad = Term.of(cpsCtx.monad)

    def shiftSelectTypeApplyApply(x:Select, targs:List[TypeTree], args: List[Term]): Term =

       val qual = x.qualifier

       def withTargs(t:Term):Term =
         if (targs.isEmpty) 
           t
         else 
           TypeApply(t, targs)


       if (qual.tpe <:< TypeRepr.of[cps.runtime.CallChainAsyncShiftSubst[F,?,?]]) then
          val shiftedName = x.name + "_shifted"
          qual.tpe.typeSymbol.method(shiftedName) match
            case Nil  => 
               throw MacroError(s"Can't find method ${shiftedName} for qual=${qual} ",posExpr(x))
            case m::Nil =>
               //withTargs(Select(qual,m))
               Apply(withTargs(Select.unique(qual,shiftedName)), args)
            case other =>
               //TODO: possible overload in later args lists.
               Select.overloaded(qual,shiftedName,targs.map(_.tpe), args) 
       else
         findObjectAsyncShiftTerm(qual) match
           case success1: ImplicitSearchSuccess =>
             val objectShift = success1.tree
             if (cpsCtx.flags.debugLevel >= 15)
               cpsCtx.log(s"found objectShift, ${success1.tree}")
               cpsCtx.log(s"objectShift.tpe = ${objectShift.tpe}")
             val shiftedExpr = Apply(
                               TypeApply(Select.unique(objectShift, "apply"),TypeTree.of[F]::Nil),
                               List(qual,monad)
                             )
             val newSelect = Select.unique(shiftedExpr, x.name)
             Apply(withTargs(newSelect), args)
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
                        TypeApply(newSelect, TypeTree.of[F]::targs).appliedTo(qual,monad).appliedToArgs(args)
                    case other =>
                        // TODO: other args in typebounds [?]
                        val expectedType = TransformUtil.createFunctionType(using qctx)(shiftedArgs.map(_.tpe), TypeBounds.empty)
                        val shiftedCaller = Select.overloaded(success2.tree, x.name, (TypeTree.of[F]::targs).map(_.tpe), List(qual,monad), expectedType)
                        Apply(shiftedCaller, args)
                             
               case failure2: ImplicitSearchFailure =>
                 throw MacroError(s"Can't find AsyncShift (${failure2.explanation}) or ObjectAsyncShift (${failure1.explanation}) for qual=${qual} ",posExpr(x))


    if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"shiftCaller, t=$term")
    term match
       case TypeApply(s@Select(qual,name),targs) =>
                  if (qual.tpe =:= monad.tpe)
                    if (s.symbol == mapSymbol || s.symbol == flatMapSymbol)
                      // TODO: add more symbols
                      findAsyncShiftTerm(qual) match
                        case shiftQual:ImplicitSearchSuccess =>
                          val newSelect = Select.unique(shiftQual.tree,s.name)
                          Apply(TypeApply(newSelect, targs).appliedTo(qual),shiftedArgs)
                        case failure:ImplicitSearchFailure =>
                          throw new MacroError(s"Can't find asyn shift for cpsMonad: ${failure.explanation}", posExpr(term))
                    else
                      throw new MacroError("Unimplemented shift for CpsMonad", posExpr(term))
                  else
                    shiftSelectTypeApplyApply(s, targs, shiftedArgs)
       case s@Select(qual,name) =>
                    shiftSelectTypeApplyApply(s, Nil, shiftedArgs)
       //case TypeApply(x, targs) =>  // now scala hvw no multiple type params
       //           Apply(TypeApply(shiftCaller(x),targs),args)
       case Apply(x, args1) =>
                  // TODO: shift args
                  Apply(shiftedApply(x,args1,args1,Set()),shiftedArgs)
       case Lambda(params, body) =>
                  val shiftedSymbols = params.zipWithIndex.filter{
                      (p,i) => shiftedIndexes.contains(i)
                  }.map{ (p,i) => p.symbol }.toSet
                  val nBody = asyncShift(body, shiftedSymbols)
                  ???
       case Block(statements, last) =>
                  Block(statements, shiftedApply(last, originArgs, shiftedArgs, shiftedIndexes))
       case _ =>
                  val errorExpr = posExpr(term)
                  throw MacroError(s"Can't shift caller ${term}",errorExpr)

  end shiftedApply

  def buildApply(cpsFun: CpsTree, fun: Term,
                 argRecords: Seq[ApplyArgRecord],
                 applyTerm: Term,
                 withAsync: Boolean,
                 withShiftedLambda: Boolean,
                 inShiftedCallChain: Boolean,
                 tails: List[Seq[ApplyArgRecord]]
                 ): CpsTree =
        if (cpsCtx.flags.debugLevel >= 15)
            cpsCtx.log(s"buildApply: fun=${safeShow(fun)}")
            cpsCtx.log(s"cpsFun.isSync=${cpsFun.isSync}, withShiftedLambda=${withShiftedLambda}, inShiftedCallChain=${inShiftedCallChain}")
        val applyTpe = applyTerm.tpe
        if (withShiftedLambda)
          if (cpsFun.isSync || inShiftedCallChain)
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
             case cs:CallChainSubstCpsTree =>
                    shiftedResultCpsTree(applyTerm, cs.shifted.appliedToArgss(argss))
             case _ =>
                    cpsFun.syncOrigin match
                       case Some(fun) =>
                          val applied = fun.appliedToArgss(argss)
                          if (inShiftedCallChain)
                             shiftedResultCpsTree(applyTerm, applied)
                          else
                             CpsTree.pure(applied, cpsFun.isChanged)
                       case None => 
                          if (inShiftedCallChain)
                             val shifted = cpsFun.transformed
                             shiftedResultCpsTree(applyTerm, shifted.appliedToArgss(argss))
                          else
                             cpsFun.monadMap(x => x.appliedToArgss(argss), applyTpe)


  def shiftedResultCpsTree(origin: Term, shifted: Term): CpsTree =
      if (shifted.tpe.isFunctionType) 
         // TODO: extract argument types. now - one experiment
         shifted.tpe match
           case AppliedType(f,List(a,AppliedType(m,b))) if f <:< TypeRepr.of[Function1] =>
             val sym = Symbol.newVal(Symbol.spliceOwner, "shiftedArg", a.widen, Flags.EmptyFlags, Symbol.noSymbol)
             AsyncLambdaCpsTree(origin, List(ValDef(sym,None)), 
                  CpsTree.impure(Apply(Select.unique(shifted,"apply"),List(Ref(sym))),b.head),origin.tpe)
           case _ =>
             throw MacroError("Async function with arity != 1 is not supported yet",posExprs(shifted,origin))
      else if (shifted.tpe <:< TypeRepr.of[cps.runtime.CallChainAsyncShiftSubst[F,?,?]]) 
         CallChainSubstCpsTree(origin, shifted, origin.tpe)
      else
         CpsTree.impure(shifted, origin.tpe)

 


  def buildShiftedApply(
                        fun: Term,
                        argRecords:Seq[ApplyArgRecord],
                        withAsync: Boolean,
                        tails:List[Seq[ApplyArgRecord]]): Term =
      val shiftedIndexes = argRecords.zipWithIndex.filter(_._1.hasShiftedLambda).map(_._2)
      val shiftedArgs = argRecords.map(_.shift().identArg(withAsync)).toList
      val originArgs = argRecords.map(_.term).toList
      if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"buildShiftedApply::argRecords=${argRecords}")
          cpsCtx.log(s"buildShiftedApply::shiftedArgs=${shiftedArgs}")
          cpsCtx.log(s"buildShiftedApply::tails=${tails}")
      val shiftedApplyHead = shiftedApply(fun, originArgs, shiftedArgs, shiftedIndexes.toSet)
      val shiftedTails = tails.map(_.map(_.shift().identArg(withAsync)).toList)
      val r = shiftedApplyHead.appliedToArgss(shiftedTails)
      if (cpsCtx.flags.debugLevel >= 15)
          cpsCtx.log(s"buildShiftedApply::r.tpe=${r.tpe}")
      r


object ApplyTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         applyTerm: qctx1.reflect.Term,
                         fun: qctx1.reflect.Term,
                         args: List[qctx1.reflect.Term]): CpsExpr[F,T] = {
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
            val treeResult = runApply(applyTerm.asInstanceOf[qctx.reflect.Term],
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


