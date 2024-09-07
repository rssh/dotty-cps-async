package cps.macros.forest

import scala.collection.immutable.Queue
import scala.quoted._
import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._
import cps.macros.forest.application.ApplicationShiftType



trait CpsTreeScope[F[_], CT, CC<:CpsMonadContext[F]] {

  cpsTreeScope: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._


  sealed abstract class CpsTree:

     def owner: Symbol

     def isAsync: Boolean

     def isChanged: Boolean

     def isLambda: Boolean

     def isSync: Boolean = ! isAsync

     def isPure: Boolean = !isAsync

     /**
      * @return transformed term, which is result of transformation.
      **/
     def transformed: Term

     /**
      * @return Some(syncTerm) if this tree is sync,( syncTerm can-be origin term, can0be not).
      *         None if this tree is async.
      **/
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
                            println(s"CpsTree.getClass ${this.getClass}")
                            println(s"candidate = $candidate")
                            throw ex;
                       }
                    else
                       // we should not cast to F[T]
                       safeSealAs[F[T]](candidate)
               CpsExpr.async[F,T](monad, sealedTransformed)
             } catch {
               case ex: Throwable =>
                 println("failed seal:"+ safeShow(transformed) )
                 println(s"transformed.tpe=${transformed.tpe}")
                 println(s"cpsTree=$this")
                 println(s"F[T]=${TypeRepr.of[F[T]]}")
                 throw ex;
             }

     def toResultWithType[T](qt: quoted.Type[T]): CpsExpr[F,T] =
             given quoted.Type[T] = qt
             toResult[T]

     def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherScope: TreeTransformScope[F1,T1,C1]): otherScope.CpsTree

     def changeOwner(newOwner: Symbol): CpsTree




  object CpsTree:

    def pure(owner: Symbol, origin:Term, isChanged: Boolean = false): CpsTree = 
      PureCpsTree(owner, origin, isChanged)

    def impure(owner: Symbol, transformed:Term, tpe: TypeRepr): CpsTree =
      AwaitSyncCpsTree(owner,transformed, tpe.widen)

    def empty: CpsTree = EmptyCpsTree


  case class PureCpsTree(override val owner: Symbol,
                         origin: qctx.reflect.Statement, 
                         isChanged: Boolean = false) extends CpsTree:

    def isAsync = false

    def isLambda = false

  
    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      MappedCpsTree(this, f, ntpe)

    //   pure(x).flatMap(f:A=>M[B])
    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      FlatMappedCpsTree(this,f, ntpe)

    def appendFinal(next: CpsTree): CpsTree =
      next match
        case BlockCpsTree(nextOwner, statements, last) =>  //dott warn here.  TODO: research
             BlockCpsTree(nextOwner, statements.prepended(origin.changeOwner(nextOwner)), last)
        case x: AsyncCpsTree =>
             BlockCpsTree(owner, Queue(origin), x.changeOwner(owner))
        case y: PureCpsTree =>
             BlockCpsTree(owner, Queue(origin), y.changeOwner(owner))
        case EmptyCpsTree =>
             this     
        case _ =>
             BlockCpsTree(owner,Queue(origin), next.changeOwner(owner))


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
                PureCpsTree(owner, Typed(t, Inferred(newOtpe)), true)
              case _ =>
                BlockCpsTree(owner, Queue(origin), PureCpsTree(owner, Typed(unitTerm,Inferred(newOtpe)),true))

    def syncOrigin: Option[Term] = 
      origin match
        case t: Term => Some(t)
        case _ => Some(Block(List(origin),unitTerm))

    def transformed: Term =
      origin match
        case t: Term =>    
          //given quotes = owner.asQuote  //  pure[Type](originTerm)(t)
          val untpureTerm = cpsCtx.monad.asTerm.select(pureSymbol)
          val tpureTerm = untpureTerm.appliedToType(otpe.widen)
          val r = Apply.copy(origin)(tpureTerm,List(t))
          r
        case s: Statement => BlockCpsTree(owner, Queue(s),CpsTree.empty).transformed

    def applyAwait(newOtpe: TypeRepr): CpsTree =
      origin match
        case t: Term =>
          AwaitSyncCpsTree(owner, t, newOtpe.widen)
        case s =>
          BlockCpsTree(owner, Queue(s),CpsTree.empty.applyAwait(newOtpe))

    override def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.PureCpsTree =
          otherCake.PureCpsTree(
            otherCake.adoptSymbol(owner),
            otherCake.adoptStatement(origin), 
            isChanged)

    override def toString(): String =
         s"PureCpsTree[$cpsTreeScope](${safeShow(origin)},${isChanged})"

    override def changeOwner(newOwner: Symbol): PureCpsTree =
        PureCpsTree(newOwner, origin.changeOwner(newOwner), isChanged)


  case object EmptyCpsTree extends CpsTree:
  
    def isAsync = false

    def isLambda = false

    def isChanged = true
  
    def owner = Symbol.spliceOwner

    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      PureCpsTree(owner,f(unitTerm), isChanged)

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
      FlatMappedCpsTree(this,f, ntpe)
    
    override def append(next: CpsTree): CpsTree = next  

    def appendFinal(next: CpsTree): CpsTree = next

    def otpe: TypeRepr = TypeRepr.of[Unit]

    def castOtpe(newOtpe: TypeRepr): CpsTree =
      PureCpsTree(owner, Typed(unitTerm, Inferred(newOtpe)), true)

    def syncOrigin: Option[Term] = Some(unitTerm)
    
    def transformed: Term = unitTerm

    def applyAwait(newOtpe: TypeRepr): CpsTree =
      // impossible
      AwaitSyncCpsTree(owner,unitTerm, newOtpe.widen)

    def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.EmptyCpsTree.type =
      otherCake.EmptyCpsTree

    // . non-important: will desappear 
    override def changeOwner(newOwner: Symbol): EmptyCpsTree.type = this
 


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



  case class AwaitSyncCpsTree(
    val owner: Symbol,
    val origin: Term, 
    val otpe: TypeRepr) extends AsyncCpsTree:

    def isLambda = false

    def transformed: Term = 
      origin

    def castOtpe(newOtpe: TypeRepr): CpsTree =
          if (otpe =:= newOtpe) then
              this
          else
              monadMap(x => Typed(x,Inferred(newOtpe)), newOtpe)

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.AwaitSyncCpsTree =
          otherCake.AwaitSyncCpsTree(otherCake.adoptSymbol(owner),
                                     otherCake.adopt(origin),
                                     otherCake.adoptType(otpe))

    override def changeOwner(newOwner: Symbol): AwaitSyncCpsTree =
          AwaitSyncCpsTree(newOwner, origin.changeOwner(newOwner), otpe)


  case class AwaitAsyncCpsTree(val nested: CpsTree, val otpe: TypeRepr) extends AsyncCpsTree:

    def owner: Symbol = nested.owner

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

    def changeOwner(newOwner: Symbol): AwaitAsyncCpsTree =
          AwaitAsyncCpsTree(nested.changeOwner(newOwner), otpe)

  // TODO: add origin for generate file positions.
  case class MappedCpsTree(prev: CpsTree, op: Term => Term, otpe: TypeRepr) extends AsyncCpsTree:

    def isLambda: Boolean = false

    def owner: Symbol = prev.owner

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
                              owner,
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

    override def changeOwner(newOwner: Symbol): MappedCpsTree =
      MappedCpsTree(prev.changeOwner(newOwner), op, otpe)



  case class FlatMappedCpsTree(
                      prev: CpsTree,
                      opm: Term => Term,
                      otpe: TypeRepr) extends AsyncCpsTree:

    def isLambda: Boolean = false

    def owner: Symbol = prev.owner

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
                  this.owner,
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

    override def changeOwner(newOwner: Symbol): FlatMappedCpsTree =
      FlatMappedCpsTree(prev.changeOwner(newOwner), opm, otpe)

  end FlatMappedCpsTree

  // TODO: refactor
  //   add origin block
  case class BlockCpsTree(
    override val owner: Symbol,
    prevs:Queue[Statement], 
    last: CpsTree) extends CpsTree:

    override def isAsync = last.isAsync

    override def isChanged = last.isChanged || !prevs.isEmpty

    override def isLambda = last.isLambda

    def toLast(f: CpsTree=>CpsTree):CpsTree =
      if (prevs.isEmpty)
        f(last)
      else
        BlockCpsTree(owner, prevs, f(last).changeOwner(owner))

    override def transformed: Term =
      if (prevs.isEmpty)
        last.transformed.changeOwner(owner)
      else
        Block(prevs.toList.map(_.changeOwner(owner)), last.transformed.changeOwner(owner))
            
        
    override def syncOrigin: Option[Term] =
      if prevs.isEmpty then
        last.syncOrigin
      else
        last.syncOrigin map (l => Block(prevs.toList,l).changeOwner(owner))


     // TODO: pass other cake ?
    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       toLast( _.monadMap(f,ntpe) )

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
       toLast(_.monadFlatMap(f,ntpe))

    def appendFinal(next: CpsTree): CpsTree =
       last.syncOrigin match
         case Some(syncLast) => BlockCpsTree(owner, prevs.appended(syncLast), next)
         case None => BlockCpsTree(owner, prevs, last.appendFinal(next))

    def otpe: TypeRepr = last.otpe

    override def rtpe: TypeRepr = last.rtpe

    override def castOtpe(ntpe: TypeRepr): CpsTree =
        BlockCpsTree(owner, prevs, last.castOtpe(ntpe))

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
        BlockCpsTree(owner, prevs, last.applyAwait(newOtpe))

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.BlockCpsTree =
        otherCake.BlockCpsTree(owner.asInstanceOf[otherCake.qctx.reflect.Symbol], 
                               prevs.asInstanceOf[Queue[otherCake.qctx.reflect.Term]], 
                               last.inCake(otherCake))

    override def changeOwner(newOwner: Symbol): BlockCpsTree = 
        BlockCpsTree(newOwner,prevs.map(_.changeOwner(newOwner)),last.changeOwner(newOwner))

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


  case class InlinedCpsTree(owner: Symbol, origin: Inlined, bindings: List[Definition],  nested: CpsTree) extends CpsTree:

    override def isAsync = nested.isAsync

    override def isChanged = nested.isChanged

    override def isLambda = nested.isLambda

    override def transformed: Term =
                  Inlined(origin.call, bindings, nested.transformed)

    override def syncOrigin: Option[Term] =
                  nested.syncOrigin.map(Inlined(origin.call, bindings, _ ))


    def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(owner, origin, bindings, nested.monadMap(f, ntpe))

    def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(owner, origin, bindings, nested.monadFlatMap(f, ntpe))

    def appendFinal(next: CpsTree): CpsTree =
         InlinedCpsTree(owner, origin, bindings, nested.appendFinal(next))

    def otpe: TypeRepr = nested.otpe

    override def rtpe: TypeRepr = nested.rtpe

    override def castOtpe(ntpe: TypeRepr): CpsTree =
         InlinedCpsTree(owner, origin, bindings, nested.castOtpe(ntpe))

    override def applyAwait(newOtpe:TypeRepr): CpsTree =
         InlinedCpsTree(owner, origin, bindings, nested.applyAwait(newOtpe))

    override def inCake[F1[_],T1, C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.InlinedCpsTree =
         otherCake.InlinedCpsTree(owner.asInstanceOf[otherCake.qctx.reflect.Symbol],
                                  origin.asInstanceOf[otherCake.qctx.reflect.Inlined], 
                                  bindings.map(_.asInstanceOf[otherCake.qctx.reflect.Definition]),
                                  nested.inCake(otherCake))

    override def changeOwner(newOwner: Symbol): InlinedCpsTree ={
      if (newOwner eq owner) then
        this
      else
        InlinedCpsTree(newOwner, origin, bindings.map(_.changeOwner(newOwner)),nested.changeOwner(newOwner))
    }


  end InlinedCpsTree

  case class ValCpsTree(owner: Symbol, valDef: ValDef, rightPart: CpsTree, nested: CpsTree, canBeLambda: Boolean = false) extends CpsTree:

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
              rightPart.changeOwner(owner).monadFlatMap(v => appendValDef(v) , nested.otpe).transformed
           else 
              rightPart.changeOwner(owner).monadMap(v => appendValDef(v) , nested.otpe).transformed

    override def syncOrigin: Option[Term] =
       for{
           rhs <- rightPart.syncOrigin
           next <- nested.syncOrigin
       } yield appendValDef(rhs)



    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
        ValCpsTree(owner, valDef, rightPart, nested.monadMap(f,ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
        ValCpsTree(owner, valDef, rightPart, nested.monadFlatMap(f,ntpe))

    def appendFinal(next: CpsTree): CpsTree = 
        ValCpsTree(owner, valDef, rightPart, nested.appendFinal(next))

    override def otpe: TypeRepr = nested.otpe

    override def rtpe: TypeRepr = nested.rtpe

    override def castOtpe(ntpe: TypeRepr): CpsTree =
        ValCpsTree(owner, valDef, rightPart, nested.castOtpe(ntpe))

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
        ValCpsTree(owner, valDef, rightPart, nested.applyAwait(newOtpe))

    def appendValDef(right: Term):Term =
       val nValDef = ValDef.copy(valDef)(name=valDef.name, 
                                         tpt=valDef.tpt, 
                                         rhs=Some(right.changeOwner(valDef.symbol)))
       val result = nested match
         case BlockCpsTree.Matcher(prevs,last) =>
           val lastTerm = last.syncOrigin.getOrElse(last.transformed)
           Block(nValDef +: prevs.toList, lastTerm.asInstanceOf[Term]).changeOwner(owner)
         case _ =>
           val next = nested.syncOrigin.getOrElse(nested.transformed)
           appendValDefToNextTerm(nValDef, next.asInstanceOf[Term])
       result

    def appendValDefToNextTerm(valDef: ValDef, next:Term): Term =
       next.changeOwner(owner) match
         case x@Lambda(params,term) => Block(List(valDef), x)
         case block@Block(stats, last) => TransformUtil.prependStatementToBlock(valDef,block)
         case other => Block(List(valDef), other)

    def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherScope: TreeTransformScope[F1,T1,C1]): otherScope.ValCpsTree =
       otherScope.ValCpsTree( owner.asInstanceOf[otherScope.qctx.reflect.Symbol],
                              valDef.asInstanceOf[otherScope.qctx.reflect.ValDef],
                              rightPart.inCake(otherScope),
                              nested.inCake(otherScope))

    def changeOwner(newOwner: Symbol): ValCpsTree =
      if (newOwner eq owner) then
        this
      else
        ValCpsTree(newOwner, valDef.changeOwner(newOwner), rightPart, nested.changeOwner(owner), canBeLambda)

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

    def owner = frs.owner

    assert(!frs.isLambda)
    assert(!snd.isLambda)

    override def transformed: Term =
         frs.appendFinal(snd).transformed

    override def syncOrigin: Option[Term] = {
       // TODO: insert warning about discarded values
       for{ x <- frs.syncOrigin
            y <- snd.syncOrigin
          } yield {
            val r = x match
              case Block(xStats, xLast) =>
                y match
                  case yBlock@Block(yStats, yLast) =>
                    TransformUtil.prependStatementsToBlock(xStats :+ xLast, yBlock.changeOwner(frs.owner))
                  case yOther =>
                    Block(xStats :+ xLast, yOther.changeOwner(frs.owner))
              case xOther =>
                y match
                  case yBlock@Block(yStats, yLast) =>
                    TransformUtil.prependStatementToBlock(xOther, yBlock.changeOwner(frs.owner))
                  case yOther =>
                    Block(xOther::Nil, yOther.changeOwner(frs.owner))
            r
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

    override def changeOwner(newOwner: Symbol): AppendCpsTree =
        if (newOwner eq owner) then
          this
        else
          AppendCpsTree(frs.changeOwner(newOwner), snd)
 
  end AppendCpsTree

  case class AsyncLambdaCpsTree(owner: Symbol,
                                originLambda: Term,
                                params: List[ValDef],
                                body: CpsTree,
                                otpe: TypeRepr) extends CpsTree:

    override def isAsync = true

    override def isChanged = true

    override def isLambda = true

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.AsyncLambdaCpsTree =
      otherCake.AsyncLambdaCpsTree(
        owner.asInstanceOf[otherCake.qctx.reflect.Symbol],
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
      val shiftedType = cpsShiftedMethodType(paramNames, paramTypes, body.otpe.asInstanceOf[TypeRepr])
       // TODO: think, maybe exists case, where we need substitute Ident(param) for x[i] (?)
       //       because otherwise it's quite strange why we have such interface in compiler

       //  r: (X1 .. XN) => F[R] = (x1 .. xN) => cps(f(x1,... xN)).
      Lambda(owner, shiftedType, (owner: Symbol, args: List[Tree]) => {
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
       AsyncLambdaCpsTree(owner, originLambda, params, body.castOtpe(ntpe), ntpe)

    // here value is discarded.
    def appendFinal(next: CpsTree): CpsTree =
      next match
        case BlockCpsTree(nextOwner, statements,last) =>  //dott warn here.  TODO: research
             BlockCpsTree(nextOwner, statements.prepended(rLambda.changeOwner(nextOwner)), last)
        case _ =>
             BlockCpsTree(owner, Queue(rLambda), next)

    override def toResult[T: quoted.Type] : CpsExpr[F,T] =
       throw MacroError("async lambda can't be result of expression", posExprs(originLambda) )

    override def toString():String =
      s"AsyncLambdaCpsTree(_,$params,$body,${otpe.show})"

    override def changeOwner(newOwner: Symbol): AsyncLambdaCpsTree = {
      if (newOwner eq owner) then
        this
      else
        AsyncLambdaCpsTree(newOwner,originLambda,params,body,otpe)
    }


  end AsyncLambdaCpsTree

  /**
   * when we have swhifted function, which should return F[A] but we
   * want to have in F[A] methods with special meaning, which should be
   * performed on F[_] before jumping into monad (exampe: Iterable.withFilter)
   * we will catch in ApplyTree such methods and substitute to appropriative calls of
   * shifted.  
   **/
  case class CallChainSubstCpsTree(owner: Symbol, origin: Term, shifted:Term, override val otpe: TypeRepr) extends CpsTree:

    def prunned: CpsTree =
      val term = Select.unique(shifted,"_finishChain")
      shiftedResultCpsTree(origin, term, None)(owner)

    override def isAsync = true

    override def isChanged = true

    override def isLambda = false

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.CallChainSubstCpsTree =
      otherCake.CallChainSubstCpsTree(
        owner.asInstanceOf[otherCake.qctx.reflect.Symbol],
        origin.asInstanceOf[otherCake.qctx.reflect.Term],
        shifted.asInstanceOf[otherCake.qctx.reflect.Term],
        otpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr]
      )

    override def changeOwner(newOwner: Symbol): CallChainSubstCpsTree =
      if (newOwner eq owner) then
        this
      else
        CallChainSubstCpsTree(newOwner,origin.changeOwner(newOwner),shifted.changeOwner(newOwner),otpe)   

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
                                     optOrigin: Option[Term],
                                     nested: CpsTree,
                                     targs:List[TypeTree],
                                     selects: List[SelectTypeApplyRecord],
                                     otpe: TypeRepr,
                                     changed: Boolean = false) extends CpsTree:

    override def isAsync = nested.isAsync

    override def isChanged = nested.isChanged || changed

    override def isLambda = nested.isLambda

    override def owner = nested.owner

    override def inCake[F1[_],T1,C1<:CpsMonadContext[F1]](otherCake: TreeTransformScope[F1,T1,C1]): otherCake.SelectTypeApplyCpsTree =
       otherCake.SelectTypeApplyCpsTree(
                               optOrigin.map(_.asInstanceOf[otherCake.qctx.reflect.Term]),
                               nested.inCake(otherCake),
                               targs.map(_.asInstanceOf[otherCake.qctx.reflect.TypeTree]),
                               selects.map(_.inCake(otherCake)),
                               otpe.asInstanceOf[otherCake.qctx.reflect.TypeRepr],
                               changed)

    override def changeOwner(newOwner: Symbol): SelectTypeApplyCpsTree = {
      if (newOwner eq owner) then
        this
      else
        SelectTypeApplyCpsTree(optOrigin, nested.changeOwner(newOwner), targs, selects, otpe, true)
    }

    def apply(term:Term):Term =
         val t1 = if (targs.isEmpty) term else term.appliedToTypeTrees(targs)
         val r = selects.foldRight(t1){ (e,s) =>
            val t2 = if (e.level == 0) then Select(s, e.symbol) else SelectOuter(s, e.symbol.name, e.level)
            if e.targs.isEmpty then t2 else TypeApply(t2, e.targs)
         }
         r.changeOwner(owner)


    override def transformed: Term =
         nested match
           case PureCpsTree(nestOwner, nestOrigin, nestChanged) =>
                optOrigin match
                  case Some(t) if (!isChanged) => t
                  case _ =>
                    nestOrigin match
                      case nestTerm: Term => PureCpsTree(owner, apply(nestTerm), isChanged).transformed
                      case s => Block(List(s), PureCpsTree(owner, apply(unitTerm), isChanged).transformed )
           case AwaitSyncCpsTree(nestOwner, nestOrigin,nestOtpe) =>
              try
                nested.monadMap(t => apply(t), otpe).transformed
              catch
                case ex:Throwable =>
                  throw ex
           case AwaitAsyncCpsTree(nNested, nOtpe) =>
             AwaitSyncCpsTree(owner, apply(nested.transformed), otpe).transformed
           case MappedCpsTree(prev, op, nOtpe: TypeRepr) =>
             MappedCpsTree(prev, t => apply(op(t)), otpe).transformed
           case FlatMappedCpsTree(prev, opm, nOtpe) =>
             FlatMappedCpsTree(prev, t => apply(opm(t)), otpe).transformed
           case AppendCpsTree(frs, snd) =>
             AppendCpsTree(frs, SelectTypeApplyCpsTree.create(None,snd,targs,selects, otpe)).transformed
           case BlockCpsTree(blockOwner, stats, last) =>
             BlockCpsTree(blockOwner, stats, SelectTypeApplyCpsTree.create(None,last,targs, selects, otpe)).transformed
           case InlinedCpsTree(owner, origin, bindings,  nested) =>
             InlinedCpsTree(owner, origin, bindings, SelectTypeApplyCpsTree.create(None, nested,targs, selects, otpe)).transformed
           case ValCpsTree(owner, valDef, rightPart, nested, canBeLambda) =>
             ValCpsTree(owner, valDef, rightPart, SelectTypeApplyCpsTree.create(None, nested, targs, selects, otpe)).transformed
           case AsyncLambdaCpsTree(owner, orig, params, body, nOtpe) =>
             throw MacroError(s"AsyncLambda can't be transformed", posExprs(Seq()++optOrigin++Some(orig) *) )
           case CallChainSubstCpsTree(owner, nestOrigin, shifted, nOtpe) =>
             if (!targs.isEmpty) then
                // targs is already should be here
                throw MacroError("CallChainSubstCpsTree already contains applied targs", posExprs(optOrigin.toSeq *))
             val revSelects = selects.reverse
             revSelects.headOption match
               case Some(head) =>
                   // TODO: add overloaded case
                   val select = Select.unique(shifted,head.symbol.name)
                   val selectTypeApply = if (head.targs.isEmpty) select else TypeApply(select,targs)
                   val nextNested = shiftedResultCpsTree(nestOrigin, selectTypeApply, None)(owner)
                   SelectTypeApplyCpsTree.create(optOrigin,nextNested,targs,revSelects.tail.reverse,otpe,true).transformed
               case None =>
                   nested.transformed
           case n@SelectTypeApplyCpsTree(nOrigin, nNested, nTargs, nSelects, nOtpe, nChanged) =>
             SelectTypeApplyCpsTree(optOrigin, nNested,targs ++ nTargs, selects ++ nSelects, otpe, changed||nChanged).transformed
           case EmptyCpsTree =>
             EmptyCpsTree.transformed

    override def typeApply(orig: Term, targs: List[qctx.reflect.TypeTree], ntpe: TypeRepr): CpsTree =
         selects match
            case head::tail =>
              if (head.targs.isEmpty) then
                 copy(optOrigin=Some(orig),
                      selects = head.copy(targs=targs)::tail,
                      otpe = ntpe)
              else
                 throw MacroError("multiple type arguments are not supported", posExprs(orig))
            case Nil =>
              if (targs.isEmpty) then // impossible,
                 copy(optOrigin=Some(orig), targs = targs)
              else
                 throw MacroError("multiple type arguments are not supported", posExprs(orig))

    override def select(orig: Term, symbol: Symbol, ntpe: TypeRepr): CpsTree =
         val newEntry = SelectTypeApplyRecord(otpe,symbol,List.empty)
         copy(optOrigin=Some(orig), selects = newEntry::selects, otpe=ntpe)


    override def syncOrigin: Option[Term] =
         if (!isChanged && optOrigin.isDefined) then
           optOrigin
         else
           nested.syncOrigin.map(t => apply(t))


    override def monadMap(f: Term => Term, ntpe: TypeRepr): CpsTree =
         nested.syncOrigin match
           case Some(t) => PureCpsTree(owner, f(apply(t)), true)
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
           case Some(t) => CpsTree.impure(owner, f(apply(t)), ntpe)
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
              PureCpsTree(owner, apply(t),isChanged).appendFinal(next)
           case None =>
              AwaitSyncCpsTree(owner, transformed, otpe).appendFinal(next)

    override def applyAwait(newOtpe: TypeRepr): CpsTree =
         nested.syncOrigin match
           case Some(t) =>
              AwaitSyncCpsTree(owner, apply(t), newOtpe)
           case None =>
              AwaitSyncCpsTree(owner, transformed, otpe).applyAwait(newOtpe)

    override def castOtpe(newOtpe: TypeRepr): CpsTree =
         nested.syncOrigin match
           case Some(t) =>
             val nTerm = optOrigin match
               case Some(origin) => Typed.copy(origin)(apply(t),Inferred(newOtpe))
               case None => Typed(apply(t),Inferred(newOtpe))
             PureCpsTree(owner, nTerm, true)
           case None => MappedCpsTree(this, x => Typed(x,Inferred(newOtpe)),newOtpe)



  end SelectTypeApplyCpsTree

  object SelectTypeApplyCpsTree:

    def create(optOrigin: Option[Term],
               nested: CpsTree,
               targs:List[TypeTree],
               selects: List[SelectTypeApplyRecord],
               otpe: TypeRepr,
               changed: Boolean = false) =
      nested match
        case prev: SelectTypeApplyCpsTree =>
             SelectTypeApplyCpsTree(optOrigin,prev.nested,targs ++ prev.targs, selects ++ prev.selects, otpe)
        case _ => SelectTypeApplyCpsTree(optOrigin,nested,targs,selects,otpe,changed)

  end SelectTypeApplyCpsTree

  extension (otherCake: TreeTransformScope[?,?,?])
    def adopt(t: qctx.reflect.Term): otherCake.qctx.reflect.Term = t.asInstanceOf[otherCake.qctx.reflect.Term]

    def adoptTerm(t: qctx.reflect.Term): otherCake.qctx.reflect.Term = t.asInstanceOf[otherCake.qctx.reflect.Term]

    def adoptType(t: qctx.reflect.TypeRepr): otherCake.qctx.reflect.TypeRepr = t.asInstanceOf[otherCake.qctx.reflect.TypeRepr]

    def adoptTermFun(op: qctx.reflect.Term => qctx.reflect.Term): otherCake.qctx.reflect.Term => otherCake.qctx.reflect.Term =
        op.asInstanceOf[otherCake.qctx.reflect.Term => otherCake.qctx.reflect.Term]

    def adoptStatement(t: qctx.reflect.Statement): otherCake.qctx.reflect.Statement = t.asInstanceOf[otherCake.qctx.reflect.Statement]
    
    def adoptSymbol(t: qctx.reflect.Symbol): otherCake.qctx.reflect.Symbol = t.asInstanceOf[otherCake.qctx.reflect.Symbol]

}
