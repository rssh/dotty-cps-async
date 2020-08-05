package cps.forest

import scala.collection.immutable.Queue
import scala.quoted._
import cps._

trait CpsTreeScope[F[_], CT] {

  cpsTreeScope: TreeTransformScope[F, CT] =>

  import qctx.tasty.{_,given _}

  sealed abstract class CpsTree:

     def isAsync: Boolean

     def isSync: Boolean = ! isAsync

     def transformed: Term

     def syncOrigin: Option[Term]

     def typeApply(targs: List[qctx.tasty.TypeTree], ntpe: Type): CpsTree =
            applyTerm(_.appliedToTypeTrees(targs), ntpe)

     def applyTerm(f: Term => Term, ntpe: Type): CpsTree

     def monadMap(f: Term => Term, ntpe:Type): CpsTree

     def monadFlatMap(f: Term => Term, ntpe:Type): CpsTree

     def append(next: CpsTree): CpsTree

     def prepend(prev: CpsTree): CpsTree =
          prev.append(this)

     /**
      * type which is 'inside ' monad, i.e. T for F[T].
      **/
     def otpe: Type

     def toResult[T: quoted.Type] : CpsExpr[F,T] =
       import cpsCtx._

       def safeSeal(t:Term):Expr[Any] =
         t.tpe.widen match
           case MethodType(_,_,_) | PolyType(_,_,_) =>
             val ext = t.etaExpand
             ext.asExpr
           case _ => t.asExpr

       syncOrigin match
         case Some(syncTerm) =>
             CpsExpr.sync(monad,safeSeal(syncTerm).asInstanceOf[Expr[T]])
         case None =>
             val sealedTransformed = safeSeal(transformed).asInstanceOf[Expr[F[T]]]
             CpsExpr.async[F,T](monad, sealedTransformed)

     def toResultWithType[T](qt: quoted.Type[T]): CpsExpr[F,T] = {
             given quoted.Type[T] = qt
             toResult[T]
     }



  object CpsTree:

    def pure(origin:Term): CpsTree = PureCpsTree(origin)

    def impure(transformed:Term, tpe: Type): CpsTree =
                   AwaitCpsTree(transformed, tpe)


  case class PureCpsTree(origin: qctx.tasty.Term) extends CpsTree:

    def isAsync = false

    def typeApply(targs: List[qctx.tasty.TypeTree]) =
                PureCpsTree(origin.appliedToTypeTrees(targs))

    def applyTerm(x: Term => Term, ntpe: Type): CpsTree =
      PureCpsTree(x(origin))

     //  pure(x).map(f) = pure(f(x))
    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
      PureCpsTree(f(origin))

    //   pure(x).flatMap(f:A=>M[B])
    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
      FlatMappedCpsTree(this,f, ntpe)

    def append(next: CpsTree): CpsTree =
      next match
        case BlockCpsTree(statements,last) =>  //dott warn here.  TODO: research
             BlockCpsTree(statements.prepended(origin), last)
        case x: AsyncCpsTree =>
             BlockCpsTree(Queue(origin), x)
        case y: PureCpsTree =>
             BlockCpsTree(Queue(origin), y)
        //case _ =>
        //    BlockCpsTree(Queue(origin), next)


    def otpe: Type = origin.tpe

    def syncOrigin: Option[Term] = Some(origin)

    def transformed: Term =
          val untpureTerm = cpsCtx.monad.asTerm.select(pureSymbol)
          val tpureTerm = untpureTerm.appliedToType(otpe)
          val r = tpureTerm.appliedTo(origin)
          r


  abstract class AsyncCpsTree extends CpsTree:

    def isAsync = true

    def transformed: Term

    def syncOrigin: Option[Term] = None

    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          MappedCpsTree(this,f, ntpe)

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          FlatMappedCpsTree(this,f, ntpe)

    def append(next: CpsTree): CpsTree =
          next match
            case syncNext: PureCpsTree => monadMap(_ => syncNext.origin, next.otpe)
            case asyncNext: AsyncCpsTree => monadFlatMap(_ => next.transformed, next.otpe)
            case blockNext: BlockCpsTree =>
                  blockNext.syncOrigin match
                    case Some(syncTerm) => monadMap(_ => syncTerm, next.otpe)
                    case None => monadFlatMap(_ => blockNext.transformed, next.otpe)



  class AwaitCpsTree(val transformed: Term, val otpe: Type) extends AsyncCpsTree:

    def applyTerm(f: Term => Term, ntpe: Type): CpsTree =
          AwaitCpsTree(f(transformed), ntpe)


  case class MappedCpsTree(prev: CpsTree, op: Term => Term, otpe: Type) extends AsyncCpsTree:

    def applyTerm(f: Term => Term, npte: Type): CpsTree =
          MappedCpsTree(prev, t => f(op(t)), npte)

    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.map(f) = prev.map(op).map(f) = prev.map(op*f)
          MappedCpsTree(prev, t => f(op(t)), ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.flatMap(f) = prev.map(op).flatMap(f) = prev.flatMap(op*f)
          FlatMappedCpsTree(prev, t => f(op(t)), ntpe)

    def transformed: Term = {
          val prevType = prev.otpe
          val untmapTerm = cpsCtx.monad.asTerm.select(mapSymbol)
          val tmapTerm = untmapTerm.appliedToTypes(List(prev.otpe,otpe))
          val r = tmapTerm.appliedToArgss(
                     List(List(prev.transformed),
                          List(
                            Lambda(
                              MethodType(List("x"))(mt => List(prev.otpe), mt => otpe),
                              opArgs => op(opArgs.head.asInstanceOf[Term])
                            )
                          )
                     )
          )
          //val r = '{
          //   ${cpsCtx.monad}.map(${prev.transformed.asExpr.asInstanceOf[F[T]]})(
          //             (x:${prev.asExpr}) => ${op('x)}
          //   )
          //}.asTerm
          r
    }


  case class FlatMappedCpsTree(
                      val prev: CpsTree,
                      opm: Term => Term,
                      otpe: Type) extends AsyncCpsTree:

    def applyTerm(f: Term => Term, npte: Type): CpsTree =
          FlatMappedCpsTree(prev, t => f(opm(t)), npte)

    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.map(f) = prev.flatMap(opm).map(f) = prev.flr(opm*f)
          FlatMappedCpsTree(prev, t => f(opm(t)), ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.flatMap(f) = prev.flatMap(opm).flatMap(f)
          FlatMappedCpsTree(this,f,ntpe)

    def transformed: Term = {
        // ${cpsCtx.monad}.flatMap(${prev.transformed})((x:${prev.it}) => ${op('x)})
        val monad = cpsCtx.monad.asTerm
        val untpFlatMapTerm = monad.select(flatMapSymbol)
        val tpFlatMapTerm = untpFlatMapTerm.appliedToTypes(List(prev.otpe,otpe))
        val r = tpFlatMapTerm.appliedToArgss(
            List(
              List(prev.transformed),
              List(
                Lambda(
                  MethodType(List("x"))(mt => List(prev.otpe),
                                        mt => AppliedType(fType.asTypeTree.tpe,List(otpe))),
                  opArgs => opm(opArgs.head.asInstanceOf[Term])
                )
             )
           )
        )
        r
    }


  end FlatMappedCpsTree

  case class BlockCpsTree(prevs:Queue[Statement], last:CpsTree) extends CpsTree:

    override def isAsync = last.isAsync

    def toLast(f:CpsTree=>CpsTree):CpsTree =
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

    def applyTerm(f: Term => Term, ntpe: Type): CpsTree =
       toLast(_.applyTerm(f,ntpe))

    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
       toLast(_.monadMap(f,ntpe))

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
       toLast(_.monadFlatMap(f,ntpe))

    def append(next: CpsTree): CpsTree =
       last.syncOrigin match
         case Some(syncLast) => BlockCpsTree(prevs.appended(syncLast),next)
         case None => BlockCpsTree(prevs, last.append(next))

    def otpe: Type = last.otpe

  end BlockCpsTree

  case class InlinedCpsTree(origin: Inlined, nested: CpsTree) extends CpsTree:

    override def isAsync = nested.isAsync

    override def transformed: Term =
                  Inlined(origin.call, origin.bindings, nested.transformed)

    override def syncOrigin: Option[Term] =
                  nested.syncOrigin.map(Inlined(origin.call, origin.bindings, _ ))

    def applyTerm(f: Term => Term, ntpe: Type): CpsTree =
         InlinedCpsTree(origin, nested.applyTerm(f, ntpe))

    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
         InlinedCpsTree(origin, nested.monadMap(f, ntpe))

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
         InlinedCpsTree(origin, nested.monadFlatMap(f, ntpe))

    def append(next: CpsTree): CpsTree =
         InlinedCpsTree(origin, nested.append(next))

    def otpe: Type = nested.otpe

  end InlinedCpsTree

  case class ValCpsTree(valDef: ValDef, rightPart: CpsTree, nested: CpsTree) extends CpsTree:

    override def isAsync = rightPart.isAsync || nested.isAsync

    override def transformed: Term =
       if (rightPart.isAsync)
         if (nested.isAsync)
             rightPart.monadFlatMap(v => appendValDef(v) , nested.otpe).transformed
         else
             rightPart.monadMap(v => appendValDef(v) , nested.otpe).transformed
       else
         appendValDef(valDef.rhs.get)

    override def syncOrigin: Option[Term] =
       for{
           rhs <- rightPart.syncOrigin
           next <- rightPart.syncOrigin
       } yield appendValDef(rhs)

    override def applyTerm(f: Term => Term, ntpe: Type): CpsTree =
        ValCpsTree(valDef, rightPart, nested.applyTerm(f,ntpe))

    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadMap(f,ntpe))

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
        ValCpsTree(valDef, rightPart, nested.monadFlatMap(f,ntpe))

    override def append(next: CpsTree): CpsTree =
        ValCpsTree(valDef, rightPart, nested.append(next))

    override def otpe: Type = nested.otpe

    def appendValDef(right: Term):Term =
       val nValDef = ValDef.copy(valDef)(name = valDef.name, tpt=valDef.tpt, rhs=Some(right))
       val result = nested match
         case BlockCpsTree( prevs,last) =>
           val lastTerm = last.syncOrigin.getOrElse(last.transformed)
           Block(nValDef +: prevs.toList, lastTerm)
         case _ =>
           val next = nested.syncOrigin.getOrElse(nested.transformed)
           appendValDefToNextTerm(nValDef, next)
       result

    def appendValDefToNextTerm(valDef: ValDef, next:Term): Term =
       next match
         case x@Lambda(params,term) => Block(List(valDef), x)
         case Block(stats, last) => Block(valDef::stats, last)
         case other => Block(List(valDef), other)


  end ValCpsTree

}
