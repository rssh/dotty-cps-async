package cps.forest

import scala.collection.immutable.Queue
import scala.quoted._
import cps._
import cps.misc._



trait CpsTreeScope[F[_], CT] {

  cpsTreeScope: TreeTransformScope[F, CT] =>

  import qctx.tasty.{_,given _}


  sealed abstract class CpsTree:

     def isAsync: Boolean

     def isChanged: Boolean

     def isSync: Boolean = ! isAsync

     def transformed: Term

     def syncOrigin: Option[Term]

     def typeApply(targs: List[qctx.tasty.TypeTree], ntpe: Type): CpsTree =
            applyTerm1(_.appliedToTypeTrees(targs), ntpe)

     def applyTerm1(f: Term => Term, ntpe: Type): CpsTree

     def select(symbol: Symbol, ntpe: Type): CpsTree

     def monadMap(f: Term => Term, ntpe:Type): CpsTree

     def monadFlatMap(f: Term => Term, ntpe:Type): CpsTree

     def append(next: CpsTree): CpsTree =
         // We should delay append resolving , to allow symbolic applying of await on sequence of appends
         AppendCpsTree(this, next)

     def appendFinal(next: CpsTree): CpsTree

     def prepend(prev: CpsTree): CpsTree =
          prev.append(this)

     def applyAwait(newOtpe: Type): CpsTree


     /**
      * type which is 'inside ' monad, i.e. T for F[T].
      **/
     def otpe: Type

     /**
      * type which we see outside. i.e. F[T] for near all 'normal' trees or X=>F[T]
      * for async lambda.
      **/
     def rtpe: Type =
        fType.unseal.tpe.appliedTo(List(otpe.widen))

     def toResult[T: quoted.Type] : CpsExpr[F,T] =
       import cpsCtx._

       def safeSeal(t:Term):Expr[Any] =
         t.tpe.widen match
           case MethodType(_,_,_) | PolyType(_,_,_) =>
             val ext = t.etaExpand
             ext.seal
           case _ => t.seal

       syncOrigin match
         case Some(syncTerm) =>
             CpsExpr.sync(monad,safeSeal(syncTerm).cast[T])
         case None =>
             val sealedTransformed = safeSeal(transformed).cast[F[T]]
             CpsExpr.async[F,T](monad, sealedTransformed)

     def toResultWithType[T](qt: quoted.Type[T]): CpsExpr[F,T] =
             given quoted.Type[T] = qt
             toResult[T]
    
     def inCake[F1[_],T1](otherScope: TreeTransformScope[F1,T1]): otherScope.CpsTree


  object CpsTree:

    def pure(origin:Term, isChanged: Boolean = false): CpsTree = PureCpsTree(origin)

    def impure(transformed:Term, tpe: Type): CpsTree =
                   AwaitSyncCpsTree(transformed, tpe.widen)


  case class PureCpsTree(origin: qctx.tasty.Term, isChanged: Boolean = false) extends CpsTree:

    def isAsync = false

    def typeApply(targs: List[qctx.tasty.TypeTree]) =
                PureCpsTree(origin.appliedToTypeTrees(targs), true)

    def applyTerm1(x: Term => Term, ntpe: Type): CpsTree =
      PureCpsTree(x(origin), isChanged)

    def select(symbol: Symbol, ntpe: Type): CpsTree =
       PureCpsTree(origin.select(symbol), isChanged)

     //  pure(x).map(f) = pure(f(x))
    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
      PureCpsTree(f(origin), isChanged)

    //   pure(x).flatMap(f:A=>M[B])
    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
      FlatMappedCpsTree(this,f, ntpe)

    def appendFinal(next: CpsTree): CpsTree =
      next match
        case BlockCpsTree(statements,last) =>  //dott warn here.  TODO: research
             BlockCpsTree(statements.prepended(origin), last)
        case x: AsyncCpsTree =>
             BlockCpsTree(Queue(origin), x)
        case y: PureCpsTree =>
             BlockCpsTree(Queue(origin), y)
        case _ =>
             BlockCpsTree(Queue(origin), next)


    def otpe: Type = origin.tpe.widen

    def syncOrigin: Option[Term] = Some(origin)

    def transformed: Term =
          val untpureTerm = cpsCtx.monad.unseal.select(pureSymbol)
          val tpureTerm = untpureTerm.appliedToType(otpe.widen)
          val r = tpureTerm.appliedTo(origin)
          r

    def applyAwait(newOtpe: Type): CpsTree =
          AwaitSyncCpsTree(origin, newOtpe)

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.PureCpsTree =
          otherCake.PureCpsTree(otherCake.adopt(origin), isChanged)

    override def toString(): String =
         s"PureCpsTree[$cpsTreeScope](${safeShow(origin)},${isChanged})"



  abstract class AsyncCpsTree extends CpsTree:

    def isAsync = true

    def isChanged = true

    def transformed: Term

    def syncOrigin: Option[Term] = None

    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          MappedCpsTree(this,f, ntpe)

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          FlatMappedCpsTree(this,f, ntpe)

    def appendFinal(next: CpsTree): CpsTree =
          val nextOtpe = next.otpe
          next match
            case syncNext: PureCpsTree => 
                            monadMap(_ => syncNext.origin, nextOtpe)
            case asyncNext: AsyncCpsTree => monadFlatMap(_ => next.transformed, nextOtpe)
            case blockNext: BlockCpsTree =>
                  blockNext.syncOrigin match
                    case Some(syncTerm) => monadMap(_ => syncTerm, nextOtpe)
                    case None => monadFlatMap(_ => blockNext.transformed, nextOtpe)

    def applyAwait(newOtpe: Type): CpsTree =
          AwaitAsyncCpsTree(this, newOtpe)


  case class AwaitSyncCpsTree(val origin: Term, val otpe: Type) extends AsyncCpsTree:

    def transformed: Term = origin

    def applyTerm1(f: Term => Term, ntpe: Type): CpsTree =
          AwaitSyncCpsTree(f(transformed), ntpe)

    def select(symbol: Symbol, ntpe: Type): CpsTree =
          AwaitSyncCpsTree(origin.select(symbol), ntpe)

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.AwaitSyncCpsTree =
          otherCake.AwaitSyncCpsTree(otherCake.adopt(origin),
                                     otherCake.adoptType(otpe))

  case class AwaitAsyncCpsTree(val nested: CpsTree, val otpe: Type) extends AsyncCpsTree:

    def transformed: Term =
      FlatMappedCpsTree(nested, (t:Term)=>t, otpe).transformed

    def select(symbol: Symbol, ntpe: Type): CpsTree =
       AwaitSyncCpsTree(transformed.select(symbol), ntpe)

    def applyTerm1(f: Term => Term, ntpe: Type): CpsTree =
          AwaitSyncCpsTree(f(transformed), ntpe)

    def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.CpsTree =
           otherCake.AwaitAsyncCpsTree(nested.inCake(otherCake), otpe.asInstanceOf[otherCake.qctx.tasty.Type])


  

  case class MappedCpsTree(prev: CpsTree, op: Term => Term, otpe: Type) extends AsyncCpsTree:


    def applyTerm1(f: Term => Term, npte: Type): CpsTree =
          MappedCpsTree(prev, t => f(op(t)), npte)

    def select(symbol: Symbol, ntpe: Type): CpsTree =
          MappedCpsTree(prev, t => op(t).select(symbol), ntpe)

    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.map(f) = prev.map(op).map(f) = prev.map(op*f)
          // TODO: rethink. Mb add val if t have multiple entries in f
          MappedCpsTree(prev, t => f(op(t)), ntpe)
          //  disabled due to https://github.com/lampepfl/dotty/issues/9254
          //MappedCpsTree(this, t=>f(t) , ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.flatMap(f) = prev.map(op).flatMap(f) = prev.flatMap(op*f)
          // FlatMappedCpsTree(prev, t => f(op(t)), ntpe)
          //  disabled due to https://github.com/lampepfl/dotty/issues/9254
          FlatMappedCpsTree(this, f, ntpe)

    def transformed: Term = {
          val untmapTerm = cpsCtx.monad.unseal.select(mapSymbol)
          val wPrevOtpe = prev.otpe.widen
          val wOtpe = otpe.widen
          val tmapTerm = untmapTerm.appliedToTypes(List(wPrevOtpe,wOtpe))
          val r = tmapTerm.appliedToArgss(
                     List(List(prev.transformed),
                          List(
                            Lambda(
                              MethodType(List("x"))(mt => List(wPrevOtpe), mt => wOtpe),
                              opArgs => op(opArgs.head.asInstanceOf[Term])
                            )
                          )
                     )
          )
          //val r = '{
          //   ${cpsCtx.monad}.map(${prev.transformed.seal.asInstanceOf[F[T]]})(
          //             (x:${prev.seal}) => ${op('x)}
          //   )
          //}.unseal
          r
    }

    override def applyAwait(newOtpe: Type): CpsTree =
       FlatMappedCpsTree(prev, op, newOtpe)

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.MappedCpsTree =
       otherCake.MappedCpsTree(prev.inCake(otherCake), 
                               op.asInstanceOf[otherCake.qctx.tasty.Term => otherCake.qctx.tasty.Term],
                               otpe.asInstanceOf[otherCake.qctx.tasty.Type])
       


  case class FlatMappedCpsTree(
                      val prev: CpsTree,
                      opm: Term => Term,
                      otpe: Type) extends AsyncCpsTree:

    def select(symbol: Symbol, ntpe: Type): CpsTree =
          FlatMappedCpsTree(prev, t => opm(t).select(symbol), ntpe)

    def applyTerm1(f: Term => Term, npte: Type): CpsTree =
          FlatMappedCpsTree(prev, t => f(opm(t)), npte)

    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.map(f) = prev.flatMap(opm).map(f) = prev.flr(opm*f)
          FlatMappedCpsTree(prev, t => f(opm(t)), ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.flatMap(f) = prev.flatMap(opm).flatMap(f)
          FlatMappedCpsTree(this,f,ntpe)

    def transformed: Term = {
        // ${cpsCtx.monad}.flatMap(${prev.transformed})((x:${prev.it}) => ${op('x)})
        val monad = cpsCtx.monad.unseal
        val untpFlatMapTerm = monad.select(flatMapSymbol)
        val wPrevOtpe = prev.otpe.widen
        val wOtpe = otpe.widen
        val tpFlatMapTerm = untpFlatMapTerm.appliedToTypes(List(wPrevOtpe,wOtpe))
        val r = tpFlatMapTerm.appliedToArgss(
            List(
              List(prev.transformed),
              List(
                Lambda(
                  MethodType(List("x"))(mt => List(wPrevOtpe),
                                        mt => fType.unseal.tpe.appliedTo(wOtpe)),
                  opArgs => opm(opArgs.head.asInstanceOf[Term])
                )
             )
           )
        )
        r
    }

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.FlatMappedCpsTree =
        otherCake.FlatMappedCpsTree(
          prev.inCake(otherCake),
          otherCake.adoptTermFun(opm),
          otherCake.adoptType(otpe)
        )

  end FlatMappedCpsTree

  // TODO: refactor
  case class BlockCpsTree(prevs:Queue[Statement], last: CpsTree) extends CpsTree:

    override def isAsync = last.isAsync

    override def isChanged = last.isChanged || !prevs.isEmpty

    def toLast(f: CpsTree=>CpsTree):CpsTree =
      if (prevs.isEmpty)
        f(last)
      else
        BlockCpsTree(prevs,f(last))

    override def transformed: Term =
      if (prevs.isEmpty)
        last.transformed
      else
        Block(prevs.toList, last.transformed)

    override def syncOrigin: Option[Term] =
      if prevs.isEmpty then
        last.syncOrigin
      else
        last.syncOrigin map (l => Block(prevs.toList,l))

    def select(symbol: Symbol, ntpe: Type): CpsTree =
       toLast(_.select(symbol,ntpe))

    def applyTerm1(f: Term => Term, ntpe: Type): CpsTree =
       toLast(_.applyTerm1(f,ntpe))

     // TODO: pass other cake ?
    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
       toLast( _.monadMap(f,ntpe) )

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
       toLast(_.monadFlatMap(f,ntpe))

    def appendFinal(next: CpsTree): CpsTree =
       last.syncOrigin match
         case Some(syncLast) => BlockCpsTree(prevs.appended(syncLast),next)
         case None => BlockCpsTree(prevs, last.appendFinal(next))

    def otpe: Type = last.otpe

    override def rtpe: Type = last.rtpe

    override def applyAwait(newOtpe: Type): CpsTree = 
        BlockCpsTree(prevs, last.applyAwait(newOtpe))

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.BlockCpsTree =
        otherCake.BlockCpsTree(prevs.asInstanceOf[Queue[otherCake.qctx.tasty.Term]], last.inCake(otherCake))

  end BlockCpsTree

  object BlockCpsTree:

     def prevsFrom(block: TreeTransformScope[?,?]#BlockCpsTree): Queue[Statement] =
           block.prevs.asInstanceOf[Queue[Statement]]

     object Matcher:

       def unapply(cpsTree: TreeTransformScope[?,?]#CpsTree): Option[(Queue[Statement], TreeTransformScope[?,?]#CpsTree)] =
            cpsTree match
              case v: BlockCpsTree =>
                Some((prevsFrom(v), v.last))
              case _ => None

    

  end BlockCpsTree


  case class InlinedCpsTree(origin: Inlined, nested: CpsTree) extends CpsTree:

    override def isAsync = nested.isAsync

    override def isChanged = nested.isChanged

    override def transformed: Term =
                  Inlined(origin.call, origin.bindings, nested.transformed)

    override def syncOrigin: Option[Term] =
                  nested.syncOrigin.map(Inlined(origin.call, origin.bindings, _ ))

    def applyTerm1(f: Term => Term, ntpe: Type): CpsTree =
         InlinedCpsTree(origin, nested.applyTerm1(f, ntpe))

    def select(symbol: Symbol, ntpe: Type): CpsTree =
         InlinedCpsTree(origin, nested.select(symbol, ntpe))

    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
         InlinedCpsTree(origin, nested.monadMap(f, ntpe))

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
         InlinedCpsTree(origin, nested.monadFlatMap(f, ntpe))

    def appendFinal(next: CpsTree): CpsTree =
         InlinedCpsTree(origin, nested.appendFinal(next))

    def otpe: Type = nested.otpe

    override def rtpe: Type = nested.rtpe

    override def applyAwait(newOtpe:Type): CpsTree = 
         InlinedCpsTree(origin, nested.applyAwait(newOtpe))

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.InlinedCpsTree =
         otherCake.InlinedCpsTree(origin.asInstanceOf[otherCake.qctx.tasty.Inlined], nested.inCake(otherCake))


  end InlinedCpsTree

  case class ValCpsTree(valDef: ValDef, rightPart: CpsTree, nested: CpsTree) extends CpsTree:

    override def isAsync = rightPart.isAsync || nested.isAsync

    override def isChanged = rightPart.isChanged || nested.isChanged

    override def transformed: Term =
       rightPart.syncOrigin match
         case Some(rhs) =>
           appendValDef(rhs)
         case None =>
           if (nested.isAsync)
              rightPart.monadFlatMap(v => appendValDef(v) , nested.otpe).transformed
           else
              rightPart.monadMap(v => appendValDef(v) , nested.otpe).transformed

    override def syncOrigin: Option[Term] =
       for{
           rhs <- rightPart.syncOrigin
           next <- nested.syncOrigin
       } yield appendValDef(rhs)
          

    def select(symbol: Symbol, ntpe: Type): CpsTree =
        ValCpsTree(valDef, rightPart, nested.select(symbol,ntpe))

    override def applyTerm1(f: Term => Term, ntpe: Type): CpsTree = 
        ValCpsTree(valDef, rightPart, nested.applyTerm1(f,ntpe))
       
    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadMap(f,ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadFlatMap(f,ntpe))

    def appendFinal(next: CpsTree): CpsTree =
        ValCpsTree(valDef, rightPart, nested.appendFinal(next))

    override def otpe: Type = nested.otpe

    override def rtpe: Type = nested.rtpe

    override def applyAwait(newOtpe: Type): CpsTree =
        ValCpsTree(valDef, rightPart, nested.applyAwait(newOtpe))

    def appendValDef(right: Term):Term =
       val nValDef = ValDef.copy(valDef)(name = valDef.name, tpt=valDef.tpt, rhs=Some(right))
       val result = nested match
         case BlockCpsTree.Matcher(prevs,last) =>
           val lastTerm = last.syncOrigin.getOrElse(last.transformed)
           Block(nValDef +: prevs.toList, lastTerm.asInstanceOf[Term])
         case _ =>
           val next = nested.syncOrigin.getOrElse(nested.transformed)
           appendValDefToNextTerm(nValDef, next.asInstanceOf[Term])
       result

    def appendValDefToNextTerm(valDef: ValDef, next:Term): Term =
       next match
         case x@Lambda(params,term) => Block(List(valDef), x)
         case Block(stats, last) => Block(valDef::stats, last)
         case other => Block(List(valDef), other)

    def inCake[F1[_],T1](otherScope: TreeTransformScope[F1,T1]): otherScope.ValCpsTree =
       otherScope.ValCpsTree(valDef.asInstanceOf[otherScope.qctx.tasty.ValDef], 
                             rightPart.inCake(otherScope), 
                             nested.inCake(otherScope))

  end ValCpsTree

  /**
   * append cps tree, which is frs and then snd.
   * we use this representation instead Mapped/Flatmapped in cases,
   * where we later can apply await to append term and simplify tree
   * instead wrapping awaited tree in extra flatMap
   */
  case class AppendCpsTree(frs: CpsTree, snd: CpsTree) extends CpsTree:

    def isAsync = frs.isAsync || snd.isAsync

    def isChanged = frs.isChanged || snd.isChanged

    override def transformed: Term =
         frs.appendFinal(snd).transformed

    override def syncOrigin: Option[Term] = {
       // TODO: insert warning about discarded values
       for{ x <- frs.syncOrigin
            y <- snd.syncOrigin
          } yield {
            x match
              case Block(xStats, xLast) =>
                y match
                  case Block(yStats, yLast) =>
                    Block((xStats :+ xLast) ++ yStats, yLast)
                  case yOther =>
                    Block(xStats :+ xLast, yOther)
              case xOther =>
                y match
                  case Block(yStats, yLast) =>
                    Block(xOther::yStats, yLast)
                  case yOther =>
                    Block(xOther::Nil, yOther)
          }
    }

    def select(symbol: Symbol, ntpe: Type): CpsTree =
         AppendCpsTree(frs, snd.select(symbol, ntpe))

    def applyTerm1(x: Term => Term, ntpe: Type): CpsTree =
         AppendCpsTree(frs, snd.applyTerm1(x, ntpe))
    
    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
         AppendCpsTree(frs, snd.monadMap(f, ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
         AppendCpsTree(frs, snd.monadFlatMap(f, ntpe))

    def appendFinal(next: CpsTree): CpsTree =
         frs.appendFinal(snd.appendFinal(next))

    override def applyAwait(newOtpe: Type): CpsTree =
         // TODO: insert optimization
         AppendCpsTree(frs, snd.applyAwait(newOtpe))

    override def otpe: Type = snd.otpe

    override def rtpe: Type = snd.rtpe

    override def inCake[F1[_],T1](otherScope: TreeTransformScope[F1,T1]): otherScope.AppendCpsTree =
         otherScope.AppendCpsTree(frs.inCake(otherScope), snd.inCake(otherScope))

  end AppendCpsTree

  case class AsyncLambdaCpsTree(originLambda: Term,
                                params: List[ValDef], 
                                body: CpsTree,
                                otpe: Type ) extends CpsTree:

    override def isAsync = true

    override def isChanged = true

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.AsyncLambdaCpsTree =
      otherCake.AsyncLambdaCpsTree(
        originLambda.asInstanceOf[otherCake.qctx.tasty.Term], 
        params.asInstanceOf[List[otherCake.qctx.tasty.ValDef]], 
        body.inCake(otherCake), 
        otpe.asInstanceOf[otherCake.qctx.tasty.Type]
      )

    override def rtpe =
      val resType = fType.unseal.tpe.appliedTo(List(otpe.widen))
      val paramTypes = params.map(_.tpt.tpe)
      if (params.length==0)
         val f0 = TypeIdent(Symbol.classSymbol("scala.Function0")).tpe
         f0.appliedTo(List(resType.widen))
      else if (params.length==1)
         val f1 = TypeIdent(Symbol.classSymbol("scala.Function1")).tpe
         f1.appliedTo( paramTypes :+ resType )
      else if (params.length==2)
         val f2 = TypeIdent(Symbol.classSymbol("scala.Function2")).tpe
         f2.appliedTo( paramTypes :+ resType )
      else
         throw MacroError("Sorry, functions with more than 2 parameters are not supported yet", posExprs(originLambda))

    def rLambda: Term =
      val paramNames = params.map(_.name)
      val paramTypes = params.map(_.tpt.tpe)
      val shiftedType = shiftedMethodType(paramNames, paramTypes, body.otpe.asInstanceOf[Type])
       // TODO: think, maybe exists case, where we need substitute Ident(param) for x[i] (?)
       //       because otherwise it's quite strange why we have such interface in compiler

       //  r: (X1 .. XN) => F[R] = (x1 .. xN) => cps(f(x1,... xN)).   
      Lambda(shiftedType, (x: List[Tree]) => { 
         // here we need to change owner of ValDefs which was in lambda.
         //  TODO: always pass mapping between new symbols as parameters to transformed
         if (cpsCtx.flags.debugLevel >= 15)
             cpsCtx.log(s"generate rLambda: params in lambda-arg = ${x}")
         val paramsMap = params.zipWithIndex.map{case (tree,index)=>(tree.symbol,index)}.toMap
         val indexedArgs = x.toIndexedSeq
         val argTransformer = new TreeMap() {
            override def transformTerm(tree: Term)(using ctx: Context): Term =
               tree match
                 case Ident(name) => paramsMap.get(tree.symbol) match
                                        case Some(index) => Ref(x(index).symbol)
                                        case _  => super.transformTerm(tree)
                 case _ => super.transformTerm(tree)
         }
         argTransformer.transformTerm(body.transformed)
      })

    override def transformed: Term = 
      // note, that type is not F[x1...xN => R]  but F[x1...xN => F[R]]
      rLambda
    
    override def syncOrigin: Option[Term] = None

    // this is select, which is applied to Function[A,B]
    // direct transforma
    //  TODO: change API to supports correct reporting
    def select(symbol: Symbol, ntpe: Type): CpsTree =
           if (symbol.name == "apply")  // TODO: check by symbol, not name
              // x=>x.apply and x
              this
           else
              throw MacroError(s"select for async lambdas is not supported yet (symbol=$symbol)", posExprs(originLambda) )
       

    // TODO: eliminate applyTerm in favor of 'Select', typeApply, Apply
    def applyTerm1(x: Term => Term, ntpe: Type): CpsTree = 
          // TODO: generate other lambda.
          throw MacroError("async lambda can't be an apply1 argument", posExprs(originLambda) )

     //  m.map(pure(x=>cosBody))(f) =  ???
    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          throw MacroError(s"attempt to monadMap AsyncLambda, f=${f}, ntpe=${ntpe.show}", posExprs(originLambda) )

    //  m.flatMap(pure(x=>cpsBody))(f)
    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          throw MacroError(s"attempt to flatMap AsyncLambda, f=${f}, ntpe=${ntpe.show}", posExprs(originLambda) )

    //  (x1,.. xM) => F[R]  can't be F[_]
    // (i.e. fixed point for F[X] = (A=>F[B]) not exists)
    override def applyAwait(newOtpe: Type): CpsTree =
       throw MacroError("async lambda can't be an await argument", posExprs(originLambda) )

    // here value is discarded. 
    def appendFinal(next: CpsTree): CpsTree =
      next match
        case BlockCpsTree(statements,last) =>  //dott warn here.  TODO: research
             BlockCpsTree(statements.prepended(rLambda), last)
        case _ =>
             BlockCpsTree(Queue(rLambda), next)

    override def toResult[T: quoted.Type] : CpsExpr[F,T] =
       throw MacroError("async lambda can't be result of expression", posExprs(originLambda) )

    override def toString():String = 
      s"AsyncLambdaCpsTree(_,$params,$body,${otpe.show})"


  end AsyncLambdaCpsTree

  /**
   * when we have swhifted function, which should return F[A] but we 
   * want to have in F[A] methods with special meaning, which should be 
   * performed on F[_] before jumping into monad (exampe: Iterable.withFilter)
   * we will catch in ApplyTree such methods and substitute to appropriative calls of
   * shifted.  When we need other
   **/
  case class CallChainSubstCpsTree(origin: Term, shifted:Term, override val otpe: Type) extends CpsTree:

    def prunned: CpsTree = 
      val term = Select.unique(shifted,"_origin")
      shiftedResultCpsTree(origin, term)

    override def isAsync = true

    override def isChanged = true

    override def inCake[F1[_],T1](otherCake: TreeTransformScope[F1,T1]): otherCake.CallChainSubstCpsTree =
      otherCake.CallChainSubstCpsTree(
        origin.asInstanceOf[otherCake.qctx.tasty.Term], 
        shifted.asInstanceOf[otherCake.qctx.tasty.Term], 
        otpe.asInstanceOf[otherCake.qctx.tasty.Type]
      )

    override def transformed: Term = 
      prunned.transformed
    
    override def syncOrigin: Option[Term] = None

    def select(symbol: Symbol, ntpe: Type): CpsTree =
      val name = symbol.name
      val select = Select.unique(shifted,name)
      shiftedResultCpsTree(origin, select)
      
    override def applyTerm1(x: Term => Term, ntpe: Type): CpsTree = 
       prunned.applyTerm1(x, ntpe)

    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
       prunned.monadMap(f, ntpe)
        
    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
       prunned.monadFlatMap(f, ntpe)

    override def appendFinal(next: CpsTree): CpsTree =
       prunned.appendFinal(next)
  
    override def applyAwait(newOtpe: Type): CpsTree =
       prunned.applyAwait(newOtpe)


  end CallChainSubstCpsTree


  extension (otherCake: TreeTransformScope[?,?]):
    def adopt(t: qctx.tasty.Term): otherCake.qctx.tasty.Term = t.asInstanceOf[otherCake.qctx.tasty.Term]

    def adoptTerm(t: qctx.tasty.Term): otherCake.qctx.tasty.Term = t.asInstanceOf[otherCake.qctx.tasty.Term]

    def adoptType(t: qctx.tasty.Type): otherCake.qctx.tasty.Type = t.asInstanceOf[otherCake.qctx.tasty.Type]

    def adoptTermFun(op: qctx.tasty.Term => qctx.tasty.Term): otherCake.qctx.tasty.Term => otherCake.qctx.tasty.Term = 
        op.asInstanceOf[otherCake.qctx.tasty.Term => otherCake.qctx.tasty.Term]



}
