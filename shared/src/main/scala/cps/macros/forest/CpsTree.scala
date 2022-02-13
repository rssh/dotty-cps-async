package cps.macros.forest

import scala.collection.immutable.Queue
import scala.quoted._
import cps._
import cps.macros._
import cps.macros.misc._



trait CpsTreeScope[F[_], CT, CC<:CpsMonadContext[F]] {

  cpsTreeScope: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._


  sealed abstract class CpsTree:

     def isAsync: Boolean

     def isChanged: Boolean

     def isLambda: Boolean

     def isSync: Boolean = ! isAsync

     def transformed: Term

     def syncOrigin: Option[Term]

     def typeApply(orig: Term, targs: List[qctx.reflect.TypeTree], ntpe: TypeRepr): CpsTree =
          SelectTypeApplyCpsTree(Some(orig), this, targs, List.empty, ntpe)

     def select(orig: Term, symbol: Symbol, ntpe: TypeRepr): CpsTree = 
          SelectTypeApplyCpsTree(Some(orig), this, List.empty, List(SelectTypeApplyRecord(otpe,symbol,List.empty)), ntpe)

     def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree

     def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree

     def append(next: CpsTree): CpsTree =
         // We should delay append resolving , to allow symbolic applying of await on sequence of appends
         AppendCpsTree(this, next)

     def appendFinal(next: CpsTree): CpsTree

     def prepend(prev: CpsTree): CpsTree =
          prev.append(this)

     def applyAwait(newOtpe: TypeRepr): CpsTree


     /**
      * type which is 'inside ' monad, i.e. T for F[T].
      **/
     def otpe: TypeRepr

     /**
      * type which we see outside. i.e. F[T] for near all 'normal' trees or X=>F[T]
      * for async lambda.
      **/
     def rtpe: TypeRepr =
        TypeRepr.of[F].appliedTo(List(otpe.widen))

     /**
      * cast CpsTree to keep newOtpe.type inside monad.
      **/
     def castOtpe(newOtpe: TypeRepr):CpsTree

     def toResult[T: quoted.Type] : CpsExpr[F,T] =
       import cpsCtx._

       def safeSealAs[T: quoted.Type](t:Term):Expr[T] =
         t.tpe.widen match
           case MethodType(_,_,_) | PolyType(_,_,_) =>
             val ext = t.etaExpand(Symbol.spliceOwner)
             ext.asExprOf[T]
           case _ => 
             t.asExprOf[T]

       syncOrigin match
         case Some(syncTerm) =>
             CpsExpr.sync(monad,safeSealAs[T](syncTerm), isChanged)
         case None =>
             try {
               val candidate = transformed
               val sealedTransformed = 
                    if (candidate.tpe <:< TypeRepr.of[F[T]]) then
                       safeSealAs[F[T]](candidate)
                    else if (otpe <:< TypeRepr.of[T]) then
                       // when F[_] is not covariant, but we know, that
                       // otpe <: T and we can map Typing somewhere inside the expression
                       try {
                         safeSealAs[F[T]](castOtpe(TypeRepr.of[T]).transformed)
                       } catch {
                         case ex: Exception =>
                            println(s"Exception during sealing to F[T], T=${TypeRepr.of[T].show}, otpe=${candidate.tpe}")
                            println(s"CpsTree.getClass ${candidate.getClass}")
                            ex.printStackTrace()
                            throw ex;
                       }
                    else
                       // we should not cast to F[T]
                       safeSealAs[F[T]](candidate)
               CpsExpr.async[F,T](monad, sealedTransformed)
             } catch {
               case ex: Throwable =>
                 println("failed seal:"+ transformed.asExpr.show )
                 println(s"transformed.tpe=${transformed.tpe}")
                 println(s"cpsTree=$this")
                 println(s"F[T]=${TypeRepr.of[F[T]]}")
                 throw ex;
             }

     def toResultWithType[T](qt: quoted.Type[T]): CpsExpr[F,T] =
             given quoted.Type[T] = qt
             toResult[T]

     def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherScope: TreeTransformScope[F1,T1,C1]): otherScope.CpsTree


  object CpsTree:

    def pure(origin:Term, isChanged: Boolean = false): CpsTree = PureCpsTree(origin, isChanged)

    def impure(transformed:Term, tpe: TypeRepr): CpsTree =
                   AwaitSyncCpsTree(transformed, tpe.widen)

    def empty: CpsTree = EmptyCpsTree


  case class PureCpsTree(origin: qctx.reflect.Statement, isChanged: Boolean = false) extends CpsTree:

    def isAsync = false

    def isLambda = false

  
    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      MappedCpsTree(this, f, ntpe)     

    //   pure(x).flatMap(f:A=>M[B])
    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
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


    def otpe: TypeRepr = 
      origin match
        case t: Term => t.tpe.widen
        case other => TypeRepr.of[Unit]

    def castOtpe(newOtpe: TypeRepr): CpsTree =
         if (newOtpe =:= otpe)
            this
         else
            origin match
              case t: Term =>
                PureCpsTree(Typed(t, Inferred(newOtpe)), true)
              case _ =>
                BlockCpsTree(Queue(origin), PureCpsTree(Typed(unitTerm,Inferred(newOtpe)),true))

    def syncOrigin: Option[Term] = 
      origin match
        case t: Term => Some(t)
        case _ => Some(Block(List(origin),unitTerm))

    def transformed: Term =
      origin match
        case t: Term =>    
          val untpureTerm = cpsCtx.monad.asTerm.select(pureSymbol)
          val tpureTerm = untpureTerm.appliedToType(otpe.widen)
          val r = tpureTerm.appliedTo(t)
          r
        case s: Statement => BlockCpsTree(Queue(s),CpsTree.empty).transformed

    def applyAwait(newOtpe: TypeRepr): CpsTree =
      origin match
        case t: Term =>
          AwaitSyncCpsTree(t, newOtpe.widen)
        case s =>
          BlockCpsTree(Queue(s),CpsTree.empty.applyAwait(newOtpe))

    override def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.PureCpsTree =
          otherCake.PureCpsTree(otherCake.adoptStatement(origin), isChanged)

    override def toString(): String =
         s"PureCpsTree[$cpsTreeScope](${safeShow(origin)},${isChanged})"


  case object EmptyCpsTree extends CpsTree:
  
    def isAsync = false

    def isLambda = false

    def isChanged = true
  
    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      PureCpsTree(f(unitTerm), isChanged)

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      FlatMappedCpsTree(this,f, ntpe)
    
    override def append(next: CpsTree): CpsTree = next  

    def appendFinal(next: CpsTree): CpsTree = next

    def otpe: TypeRepr = TypeRepr.of[Unit]

    def castOtpe(newOtpe: TypeRepr): CpsTree =
      PureCpsTree(Typed(unitTerm, Inferred(newOtpe)), true)

    def syncOrigin: Option[Term] = Some(unitTerm)
    
    def transformed: Term = unitTerm

    def applyAwait(newOtpe: TypeRepr): CpsTree =
      // impossible
      AwaitSyncCpsTree(unitTerm, newOtpe.widen)

    def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.EmptyCpsTree.type =
      otherCake.EmptyCpsTree



  abstract sealed class AsyncCpsTree extends CpsTree:

    def isAsync = true

    def isChanged = true

    def transformed: Term

    def syncOrigin: Option[Term] = None

    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          MappedCpsTree(this,f, ntpe)

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          FlatMappedCpsTree(this,f, ntpe)

    def appendFinal(next: CpsTree): CpsTree =
          val nextOtpe = next.otpe
          next match
            case syncNext: PureCpsTree =>
                syncNext.origin match
                  case t: Term => monadMap( _ => t, nextOtpe) 
                  case s =>  monadMap( _ => Block(List(s),unitTerm), nextOtpe)
            case asyncNext: AsyncCpsTree => monadFlatMap(_ => next.transformed, nextOtpe)
            case EmptyCpsTree => this
            case _ =>
                  next.syncOrigin match
                    case Some(syncTerm) => monadMap(_ => syncTerm, nextOtpe)
                    case None => monadFlatMap(_ => next.transformed, nextOtpe)

    def applyAwait(newOtpe: TypeRepr): CpsTree =
          AwaitAsyncCpsTree(this, newOtpe)



  case class AwaitSyncCpsTree(val origin: Term, val otpe: TypeRepr) extends AsyncCpsTree:

    def isLambda = false

    def transformed: Term = 
      origin

    def castOtpe(newOtpe: TypeRepr): CpsTree =
          if (otpe =:= newOtpe) then
              this
          else
              monadMap(x => Typed(x,Inferred(newOtpe)), newOtpe)

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.AwaitSyncCpsTree =
          otherCake.AwaitSyncCpsTree(otherCake.adopt(origin),
                                     otherCake.adoptType(otpe))

  case class AwaitAsyncCpsTree(val nested: CpsTree, val otpe: TypeRepr) extends AsyncCpsTree:

    def isLambda: Boolean = false

    def transformed: Term =
      FlatMappedCpsTree(nested, (t:Term)=>t, otpe).transformed


    def castOtpe(ntpe: TypeRepr): CpsTree =
          if (otpe =:= ntpe)
             this
          else
             MappedCpsTree(this, t => Typed(t,Inferred(ntpe)), ntpe)


    def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.CpsTree =
           otherCake.AwaitAsyncCpsTree(nested.inCake(otherCake), otpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr])




  case class MappedCpsTree(prev: CpsTree, op: Term => Term, otpe: TypeRepr) extends AsyncCpsTree:

    def isLambda: Boolean = false


    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.map(f) = prev.map(op).map(f) = prev.map(op*f)
          // TODO: rethink. Mb add val if t have multiple entries in f
          MappedCpsTree(prev, t => f(op(t)), ntpe)
          //  disabled due to https://github.com/lampepfl/dotty/issues/9254
          //MappedCpsTree(this, t=>f(t) , ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.flatMap(f) = prev.map(op).flatMap(f) = prev.flatMap(op*f)
          // FlatMappedCpsTree(prev, t => f(op(t)), ntpe)
          //  disabled due to https://github.com/lampepfl/dotty/issues/9254
          FlatMappedCpsTree(this, f, ntpe)

    def transformed: Term = {
          val untmapTerm = cpsCtx.monad.asTerm.select(mapSymbol)
          val wPrevOtpe = TransformUtil.veryWiden(prev.otpe.widen)
          val tmapTerm = untmapTerm.appliedToTypes(List(wPrevOtpe,otpe))
          val r = tmapTerm.appliedToArgss(
                     List(List(prev.transformed.changeOwner(Symbol.spliceOwner)),
                          List(
                            Lambda(
                              Symbol.spliceOwner,
                              MethodType(List("x"))(mt => List(wPrevOtpe), mt => otpe),
                              (owner, opArgs) => op(opArgs.head.asInstanceOf[Term]).changeOwner(owner)
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

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
       FlatMappedCpsTree(prev, op, newOtpe)

    def castOtpe(newOtpe: TypeRepr): CpsTree =
       MappedCpsTree(prev, term => Typed(op(term),Inferred(newOtpe)), newOtpe)


    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.MappedCpsTree =
       otherCake.MappedCpsTree(prev.inCake(otherCake),
                               op.asInstanceOf[otherCake.qctx.reflect.Term => otherCake.qctx.reflect.Term],
                               otpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr])



  case class FlatMappedCpsTree(
                      val prev: CpsTree,
                      opm: Term => Term,
                      otpe: TypeRepr) extends AsyncCpsTree:

    def isLambda: Boolean = false



    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.map(f) = prev.flatMap(opm).map(f) = prev.flr(opm*f)
          FlatMappedCpsTree(prev, t => f(opm(t)), ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          // this.flatMap(f) = prev.flatMap(opm).flatMap(f)
          FlatMappedCpsTree(this,f,ntpe)

    def castOtpe(ntpe: TypeRepr): CpsTree =
          MappedCpsTree(this, t => Typed(t,Inferred(ntpe)), ntpe)

    def transformed: Term = {
        // ${cpsCtx.monad}.flatMap(${prev.transformed})((x:${prev.it}) => ${op('x)})
        val monad = cpsCtx.monad.asTerm
        val untpFlatMapTerm = monad.select(flatMapSymbol)
        //val wPrevOtpe = prev.otpe.widen
        val wPrevOtpe = TransformUtil.veryWiden(prev.otpe)
        //val wOtpe = otpe.widen
        val wOtpe = TransformUtil.veryWiden(otpe)
        val tpFlatMapTerm = untpFlatMapTerm.appliedToTypes(List(wPrevOtpe,wOtpe))
        val r = tpFlatMapTerm.appliedToArgss(
            List(
              List(prev.castOtpe(wPrevOtpe).transformed.changeOwner(Symbol.spliceOwner)),
              List(
                Lambda(
                  Symbol.spliceOwner,
                  MethodType(List("x"))(mt => List(wPrevOtpe),
                                        mt => TypeRepr.of[F].appliedTo(wOtpe)),
                  (owner,opArgs) => opm(opArgs.head.asInstanceOf[Term]).changeOwner(owner)
                )
             )
           )
        )
        r
    }

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.FlatMappedCpsTree =
        otherCake.FlatMappedCpsTree(
          prev.inCake(otherCake),
          otherCake.adoptTermFun(opm),
          otherCake.adoptType(otpe)
        )

  end FlatMappedCpsTree

  // TODO: refactor
  //   add origin block
  case class BlockCpsTree(prevs:Queue[Statement], last: CpsTree) extends CpsTree:

    override def isAsync = last.isAsync

    override def isChanged = last.isChanged || !prevs.isEmpty

    override def isLambda = last.isLambda

    def toLast(f: CpsTree=>CpsTree):CpsTree =
      if (prevs.isEmpty)
        f(last)
      else
        BlockCpsTree(prevs,f(last))

    override def transformed: Term =
      if (prevs.isEmpty)
        last.transformed
      else
        Block(prevs.toList, last.transformed).changeOwner(Symbol.spliceOwner)

    override def syncOrigin: Option[Term] =
      if prevs.isEmpty then
        last.syncOrigin
      else
        last.syncOrigin map (l => Block(prevs.toList,l).changeOwner(Symbol.spliceOwner))


     // TODO: pass other cake ?
    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       toLast( _.monadMap(f,ntpe) )

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       toLast(_.monadFlatMap(f,ntpe))

    def appendFinal(next: CpsTree): CpsTree =
       last.syncOrigin match
         case Some(syncLast) => BlockCpsTree(prevs.appended(syncLast),next)
         case None => BlockCpsTree(prevs, last.appendFinal(next))

    def otpe: TypeRepr = last.otpe

    override def rtpe: TypeRepr = last.rtpe

    override def castOtpe(ntpe: TypeRepr): CpsTree =
        BlockCpsTree(prevs, last.castOtpe(ntpe))

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
        BlockCpsTree(prevs, last.applyAwait(newOtpe))

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.BlockCpsTree =
        otherCake.BlockCpsTree(prevs.asInstanceOf[Queue[otherCake.qctx.reflect.Term]], last.inCake(otherCake))

  end BlockCpsTree

  object BlockCpsTree:

     def prevsFrom(block: TreeTransformScope[?,?,?]#BlockCpsTree): Queue[Statement] =
           block.prevs.asInstanceOf[Queue[Statement]]

     object Matcher:

       def unapply(cpsTree: TreeTransformScope[?,?,?]#CpsTree): Option[(Queue[Statement], TreeTransformScope[?,?,?]#CpsTree)] =
            cpsTree match
              case v: BlockCpsTree =>
                Some((prevsFrom(v), v.last))
              case _ => None



  end BlockCpsTree


  case class InlinedCpsTree(origin: Inlined, bindings: List[Definition],  nested: CpsTree) extends CpsTree:

    override def isAsync = nested.isAsync

    override def isChanged = nested.isChanged

    override def isLambda = nested.isLambda

    override def transformed: Term =
                  Inlined(origin.call, bindings, nested.transformed)

    override def syncOrigin: Option[Term] =
                  nested.syncOrigin.map(Inlined(origin.call, bindings, _ ))


    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(origin, bindings, nested.monadMap(f, ntpe))

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(origin, bindings, nested.monadFlatMap(f, ntpe))

    def appendFinal(next: CpsTree): CpsTree =
         InlinedCpsTree(origin, bindings, nested.appendFinal(next))

    def otpe: TypeRepr = nested.otpe

    override def rtpe: TypeRepr = nested.rtpe

    override def castOtpe(ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(origin, bindings, nested.castOtpe(ntpe))

    override def applyAwait(newOtpe:TypeRepr): CpsTree =
         InlinedCpsTree(origin, bindings, nested.applyAwait(newOtpe))

    override def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.InlinedCpsTree =
         otherCake.InlinedCpsTree(origin.asInstanceOf[otherCake.qctx.reflect.Inlined], 
                                  bindings.map(_.asInstanceOf[otherCake.qctx.reflect.Definition]),
                                  nested.inCake(otherCake))


  end InlinedCpsTree

  case class ValCpsTree(valDef: ValDef, rightPart: CpsTree, nested: CpsTree, canBeLambda: Boolean = false) extends CpsTree:

    if (rightPart.isLambda)
       if (cpsCtx.flags.debugLevel > 10) then
           cpsCtx.log("Creating ValDef with right-part: lambda.")
           cpsCtx.log(s"rightPart: $rightPart")
           cpsCtx.log(s"rightPart.transformed: ${rightPart.transformed}")
       if (!canBeLambda) then
           val posTerm = valDef.rhs.getOrElse(
                            rightPart match
                              case r: AsyncLambdaCpsTree => r.originLambda
                              case _ => Block(List(valDef),Literal(UnitConstant()))
                         )
           throw new MacroError("Creating ValDef with lambda in right-part",posExprs(posTerm))
       

    override def isAsync = rightPart.isAsync || nested.isAsync

    override def isChanged = rightPart.isChanged || nested.isChanged

    override def isLambda = rightPart.isLambda || nested.isLambda

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



    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadMap(f,ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadFlatMap(f,ntpe))

    def appendFinal(next: CpsTree): CpsTree = 
        ValCpsTree(valDef, rightPart, nested.appendFinal(next))

    override def otpe: TypeRepr = nested.otpe

    override def rtpe: TypeRepr = nested.rtpe

    override def castOtpe(ntpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.castOtpe(ntpe))

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
        ValCpsTree(valDef, rightPart, nested.applyAwait(newOtpe))

    def appendValDef(right: Term):Term =
       val nValDef = ValDef.copy(valDef)(name = valDef.name, 
                                         tpt=valDef.tpt, 
                                         rhs=Some(right.changeOwner(valDef.symbol)))
       val result = nested match
         case BlockCpsTree.Matcher(prevs,last) =>
           val lastTerm = last.syncOrigin.getOrElse(last.transformed)
           Block(nValDef +: prevs.toList, lastTerm.asInstanceOf[Term]).changeOwner(Symbol.spliceOwner)
         case _ =>
           val next = nested.syncOrigin.getOrElse(nested.transformed)
           appendValDefToNextTerm(nValDef, next.asInstanceOf[Term])
       result

    def appendValDefToNextTerm(valDef: ValDef, next:Term): Term =
       next.changeOwner(valDef.symbol.owner) match
         case x@Lambda(params,term) => Block(List(valDef), x)
         case Block(stats, last) => Block(valDef::stats, last)
         case other => Block(List(valDef), other)

    def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherScope: TreeTransformScope[F1,T1,C1]): otherScope.ValCpsTree =
       otherScope.ValCpsTree(valDef.asInstanceOf[otherScope.qctx.reflect.ValDef],
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

    def isLambda = false

    assert(!frs.isLambda)
    assert(!snd.isLambda)

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
                    Block((xStats :+ xLast) ++ yStats, yLast).changeOwner(Symbol.spliceOwner)
                  case yOther =>
                    Block(xStats :+ xLast, yOther).changeOwner(Symbol.spliceOwner)
              case xOther =>
                y match
                  case Block(yStats, yLast) =>
                    Block(xOther::yStats, yLast).changeOwner(Symbol.spliceOwner)
                  case yOther =>
                    Block(xOther::Nil, yOther).changeOwner(Symbol.spliceOwner)
          }
    }


    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         AppendCpsTree(frs, snd.monadMap(f, ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         AppendCpsTree(frs, snd.monadFlatMap(f, ntpe))

    def appendFinal(next: CpsTree): CpsTree =
         frs.appendFinal(snd.appendFinal(next))

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
         // TODO: insert optimization
         AppendCpsTree(frs, snd.applyAwait(newOtpe))

    override def otpe: TypeRepr = snd.otpe

    override def rtpe: TypeRepr = snd.rtpe

    override def castOtpe(ntpe: TypeRepr): CpsTree =
         AppendCpsTree(frs, snd.castOtpe(ntpe))

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherScope: TreeTransformScope[F1,T1,C1]): otherScope.AppendCpsTree =
         otherScope.AppendCpsTree(frs.inCake(otherScope), snd.inCake(otherScope))

  end AppendCpsTree

  case class AsyncLambdaCpsTree(originLambda: Term,
                                params: List[ValDef],
                                body: CpsTree,
                                otpe: TypeRepr ) extends CpsTree:

    override def isAsync = true

    override def isChanged = true

    override def isLambda = true

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.AsyncLambdaCpsTree =
      otherCake.AsyncLambdaCpsTree(
        originLambda.asInstanceOf[otherCake.qctx.reflect.Term],
        params.asInstanceOf[List[otherCake.qctx.reflect.ValDef]],
        body.inCake(otherCake),
        otpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr]
      )

    override def rtpe =
      val resType = TypeRepr.of[F].appliedTo(List(otpe.widen))
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
      else if (params.length==3)
         val f3 = TypeIdent(Symbol.classSymbol("scala.Function3")).tpe
         f3.appliedTo( paramTypes :+ resType )
      else
         throw MacroError("Sorry, functions with more than 3 parameters are not supported yet", posExprs(originLambda))

    def rLambda: Term =
      val paramNames = params.map(_.name)
      val paramTypes = params.map(_.tpt.tpe)
      val shiftedType = shiftedMethodType(paramNames, paramTypes, body.otpe.asInstanceOf[TypeRepr])
       // TODO: think, maybe exists case, where we need substitute Ident(param) for x[i] (?)
       //       because otherwise it's quite strange why we have such interface in compiler

       //  r: (X1 .. XN) => F[R] = (x1 .. xN) => cps(f(x1,... xN)).
      Lambda(Symbol.spliceOwner, shiftedType, (owner: Symbol, args: List[Tree]) => {
         // here we need to change owner of ValDefs which was in lambda.
         //  TODO: always pass mapping between new symbols as parameters to transformed
         if (cpsCtx.flags.debugLevel >= 15)
             cpsCtx.log(s"generate rLambda: params in lambda-arg = ${args}")
         TransformUtil.substituteLambdaParams(params, args, body.transformed, owner).changeOwner(owner)
      })

    override def transformed: Term =
      // note, that type is not F[x1...xN => R]  but F[x1...xN => F[R]]
      rLambda

    override def syncOrigin: Option[Term] = None

    // this is select, which is applied to Function[A,B]
    // direct transforma
    //  TODO: change API to supports correct reporting
    override def select(origin: Term, symbol: Symbol, ntpe: TypeRepr): CpsTree =
           if (symbol.name == "apply")  // TODO: check by symbol, not name
              // x=>x.apply and x
              this
           else
              throw MacroError(s"select for async lambdas is not supported yet (symbol=$symbol)", posExprs(origin, originLambda) )



     //  m.map(pure(x=>cosBody))(f) =  ???
    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          throw MacroError(s"attempt to monadMap AsyncLambda, f=${f}, ntpe=${ntpe.show}", posExprs(originLambda) )

    //  m.flatMap(pure(x=>cpsBody))(f)
    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
          throw MacroError(s"attempt to flatMap AsyncLambda, f=${f}, ntpe=${ntpe.show}", posExprs(originLambda) )

    //  (x1,.. xM) => F[R]  can't be F[_]
    // (i.e. fixed point for F[X] = (A=>F[B]) not exists)
    override def applyAwait(newOtpe: TypeRepr): CpsTree =
       throw MacroError("async lambda can't be an await argument", posExprs(originLambda) )

    override def castOtpe(ntpe: TypeRepr): CpsTree =
       AsyncLambdaCpsTree(originLambda, params, body.castOtpe(ntpe), ntpe)

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
   * shifted.  
   **/
  case class CallChainSubstCpsTree(origin: Term, shifted:Term, override val otpe: TypeRepr) extends CpsTree:

    def prunned: CpsTree =
      val term = Select.unique(shifted,"_finishChain")
      shiftedResultCpsTree(origin, term)

    override def isAsync = true

    override def isChanged = true

    override def isLambda = false

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.CallChainSubstCpsTree =
      otherCake.CallChainSubstCpsTree(
        origin.asInstanceOf[otherCake.qctx.reflect.Term],
        shifted.asInstanceOf[otherCake.qctx.reflect.Term],
        otpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr]
      )

    override def transformed: Term =
      prunned.transformed

    override def syncOrigin: Option[Term] = None


    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       prunned.monadMap(f, ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       prunned.monadFlatMap(f, ntpe)

    override def appendFinal(next: CpsTree): CpsTree =
       prunned.appendFinal(next)

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
       prunned.applyAwait(newOtpe)

    override def castOtpe(newOtpe: TypeRepr): CpsTree =
       prunned.castOtpe(newOtpe)

  end CallChainSubstCpsTree

  case class SelectTypeApplyRecord(prevTpe: TypeRepr, symbol: Symbol, targs: List[TypeTree], level: Int = 0):
     def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.SelectTypeApplyRecord =
         otherCake.SelectTypeApplyRecord(
                                        prevTpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr],
                                        symbol.asInstanceOf[otherCake.qctx.reflect.Symbol],
                                        targs.map(_.asInstanceOf[otherCake.qctx.reflect.TypeTree]))
                                         

  /**
   * represent select expression, which can be in monad or outside monad.
   *
   *  constructor is not devoted to used directly: use SelectTypeApplyCpsTree.create instead.
   *  selects is reversed (i.e. external added to head)
   **/
  case class SelectTypeApplyCpsTree(
                                    origin: Option[Term],
                                    nested: CpsTree, 
                                    targs:List[TypeTree], 
                                    selects: List[SelectTypeApplyRecord],
                                    otpe: TypeRepr,
                                    changed: Boolean = false) extends CpsTree:

    override def isAsync = nested.isAsync

    override def isChanged = nested.isChanged || changed

    override def isLambda = nested.isLambda

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.SelectTypeApplyCpsTree =
       otherCake.SelectTypeApplyCpsTree(
                               origin.map(_.asInstanceOf[otherCake.qctx.reflect.Term]),
                               nested.inCake(otherCake),
                               targs.map(_.asInstanceOf[otherCake.qctx.reflect.TypeTree]),
                               selects.map(_.inCake(otherCake)),
                               otpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr],
                               changed)

    def apply(term:Term):Term =
         val t1 = if (targs.isEmpty) term else term.appliedToTypeTrees(targs)
         selects.foldRight(t1){ (e,s) =>
            val t2 = if (e.level == 0) then Select(s, e.symbol) else SelectOuter(s, e.symbol.name, e.level)
            if e.targs.isEmpty then t2 else TypeApply(t2, e.targs)
         }
         

    override def transformed: Term =
         nested match
           case PureCpsTree(nestOrigin, nestChanged) =>
                origin match
                  case Some(t) if (!isChanged) => t
                  case None => 
                    nestOrigin match
                      case nestTerm: Term => PureCpsTree(apply(nestTerm), isChanged).transformed
                      case s => Block(List(s), PureCpsTree(apply(unitTerm), isChanged).transformed )
           case AwaitSyncCpsTree(nestOrigin,nestOtpe) =>
             nested.monadMap(t => apply(t), otpe).transformed
           case AwaitAsyncCpsTree(nNested, nOtpe) =>
             AwaitSyncCpsTree(apply(nested.transformed), otpe).transformed
           case MappedCpsTree(prev, op, nOtpe: TypeRepr) =>
             MappedCpsTree(prev, t => apply(op(t)), otpe).transformed
           case FlatMappedCpsTree(prev, opm, nOtpe) =>
             FlatMappedCpsTree(prev, t => apply(opm(t)), otpe).transformed
           case AppendCpsTree(frs, snd) =>
             AppendCpsTree(frs, SelectTypeApplyCpsTree.create(None,snd,targs,selects, otpe)).transformed
           case BlockCpsTree(stats, last) =>
             BlockCpsTree(stats, SelectTypeApplyCpsTree.create(None,last,targs, selects, otpe)).transformed
           case InlinedCpsTree(origin, bindings,  nested) =>
             InlinedCpsTree(origin, bindings, SelectTypeApplyCpsTree.create(None, nested,targs, selects, otpe)).transformed
           case ValCpsTree(valDef, rightPart, nested, canBeLambda) =>
             ValCpsTree(valDef, rightPart, SelectTypeApplyCpsTree.create(None, nested, targs, selects, otpe)).transformed
           case AsyncLambdaCpsTree(orig, params, body, nOtpe) =>
             throw MacroError(s"AsyncLambda can't be transformed", posExprs(Seq()++origin++Some(orig):_*) )
           case CallChainSubstCpsTree(nestOrigin, shifted, nOtpe) =>
             if (!targs.isEmpty) then
                // targs is already should be here
                throw MacroError("CallChainSubstCpsTree already contains applied targs", posExprs(origin.toSeq: _*))
             val revSelects = selects.reverse
             revSelects.headOption match
               case Some(head) =>
                   // TODO: add overloaded case
                   val select = Select.unique(shifted,head.symbol.name)
                   val selectTypeApply = if (head.targs.isEmpty) select else TypeApply(select,targs)
                   val nextNested = shiftedResultCpsTree(nestOrigin, selectTypeApply)
                   SelectTypeApplyCpsTree.create(origin,nextNested,targs,revSelects.tail.reverse,otpe,true).transformed
               case None =>
                   nested.transformed
           case n@SelectTypeApplyCpsTree(nOrigin, nNested, nTargs, nSelects, nOtpe, nChanged) =>
             SelectTypeApplyCpsTree(origin, nNested,targs ++ nTargs, selects ++ nSelects, otpe, changed||nChanged).transformed
           case EmptyCpsTree =>
             EmptyCpsTree.transformed

    override def typeApply(orig: Term, targs: List[qctx.reflect.TypeTree], ntpe: TypeRepr): CpsTree =
         selects match
            case head::tail =>
              if (head.targs.isEmpty) then
                 copy(origin=Some(orig),
                      selects = head.copy(targs=targs)::tail,
                      otpe = ntpe)
              else
                 throw MacroError("multiple type arguments are not supported", posExprs(orig))
            case Nil =>
              if (targs.isEmpty) then // impossible, 
                 copy(origin=Some(orig), targs = targs)
              else
                 throw MacroError("multiple type arguments are not supported", posExprs(orig))
            
    override def select(orig: Term, symbol: Symbol, ntpe: TypeRepr): CpsTree = 
         val newEntry = SelectTypeApplyRecord(otpe,symbol,List.empty)
         copy(origin=Some(orig), selects = newEntry::selects, otpe=ntpe)


    override def syncOrigin: Option[Term] = 
         if (!isChanged && origin.isDefined) then
           origin
         else
           nested.syncOrigin.map(t => apply(t))


    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         nested.syncOrigin match
           case Some(t) => PureCpsTree(f(apply(t)), true)
           case None => 
                  nested match
                    case MappedCpsTree(prevNested, prevF, prevNtpe) =>
                       MappedCpsTree(prevNested,
                                     x => f(apply(prevF(x))),
                                     ntpe)
                    case _ =>
                       //MappedCpsTree(this, f, ntpe)
                       MappedCpsTree(nested, t => f(apply(t)), ntpe)
           

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         nested.syncOrigin match
           case Some(t) => CpsTree.impure(f(apply(t)), ntpe)
           case None => 
                  nested match
                    case MappedCpsTree(prevNested, prevF, prevNtpe) =>
                           FlatMappedCpsTree(prevNested,
                                             x => f(apply(prevF(x))),
                                             ntpe)
                    case _ =>
                           FlatMappedCpsTree(this, f, ntpe)


    override def appendFinal(next: CpsTree): CpsTree =
         nested.syncOrigin match
           case Some(t) =>
              PureCpsTree(apply(t),isChanged).appendFinal(next)
           case None =>
              AwaitSyncCpsTree(transformed, otpe).appendFinal(next)
             
    override def applyAwait(newOtpe: TypeRepr): CpsTree =
         nested.syncOrigin match
           case Some(t) =>
              AwaitSyncCpsTree(apply(t), newOtpe)
           case None =>
              AwaitSyncCpsTree(transformed, otpe).applyAwait(newOtpe)
              
    override def castOtpe(newOtpe: TypeRepr): CpsTree =
         nested.syncOrigin match
           case Some(t) => PureCpsTree(Typed(apply(t),Inferred(newOtpe)),true)
           case None => MappedCpsTree(this, x => Typed(x,Inferred(newOtpe)),newOtpe)



  end SelectTypeApplyCpsTree

  object SelectTypeApplyCpsTree:

    def create(origin: Option[Term],
               nested: CpsTree, 
               targs:List[TypeTree], 
               selects: List[SelectTypeApplyRecord],
               otpe: TypeRepr,
               changed: Boolean = false) =
      nested match
        case prev: SelectTypeApplyCpsTree =>
             SelectTypeApplyCpsTree(origin,prev.nested,targs ++ prev.targs, selects ++ prev.selects, otpe)
        case _ => SelectTypeApplyCpsTree(origin,nested,targs,selects,otpe,changed)

  end SelectTypeApplyCpsTree

  extension (otherCake: TreeTransformScope[?,?,?])
    def adopt(t: qctx.reflect.Term): otherCake.qctx.reflect.Term = t.asInstanceOf[otherCake.qctx.reflect.Term]

    def adoptTerm(t: qctx.reflect.Term): otherCake.qctx.reflect.Term = t.asInstanceOf[otherCake.qctx.reflect.Term]

    def adoptType(t: qctx.reflect.TypeRepr): otherCake.qctx.reflect.TypeRepr = t.asInstanceOf[otherCake.qctx.reflect.TypeRepr]

    def adoptTermFun(op: qctx.reflect.Term => qctx.reflect.Term): otherCake.qctx.reflect.Term => otherCake.qctx.reflect.Term =
        op.asInstanceOf[otherCake.qctx.reflect.Term => otherCake.qctx.reflect.Term]

    def adoptStatement(t: qctx.reflect.Statement): otherCake.qctx.reflect.Statement = t.asInstanceOf[otherCake.qctx.reflect.Statement]
    


}
