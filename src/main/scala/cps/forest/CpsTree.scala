package cps.forest

import scala.quoted._
import cps._

trait CpsTreeScope[F[_], CT] {

  cpsTreeScope: TreeTransformScope[F, CT] =>

  import qctx.tasty.{_,given _}

  sealed abstract class CpsTree:

     def isAsync: Boolean

     def isSync: Boolean = ! isAsync

     def transformed: Term

     def typeApply(targs: List[qctx.tasty.TypeTree], ntpe: Type): CpsTree =
           applyTerm(_.appliedToTypeTrees(targs), ntpe)

     def applyTerm(f: Term => Term, ntpe: Type): CpsTree

     def monadMap(f: Term => Term, ntpe:Type): CpsTree

     def monadFlatMap(f: Term => Term, ntpe:Type): CpsTree

     /**
      * type which is 'inside ' monad, i.e. T for F[T].
      **/
     def otpe: Type

     def toResult[T: quoted.Type](origin: Expr[T]) : CpsExpr[F,T] =
       import cpsCtx._
       this match
         case syncTerm: PureCpsTree =>
             val code = safeSeal(syncTerm.origin).asInstanceOf[Expr[T]]
             CpsExpr.sync(monad,code)
         case cpsTree: AsyncCpsTree =>
             val transformed = safeSeal(cpsTree.transformed).asInstanceOf[Expr[F[T]]]
             CpsExpr.async[F,T](monad, transformed)

     def safeSeal(t:Term): Expr[Any] =
       t.tpe.widen match 
         case _ : MethodType | _ : PolyType => 
           val etaExpanded = t.etaExpand
           etaExpanded.seal
         case _ => t.seal
            


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

    def otpe: Type = origin.tpe

    def transformed: Term = 
          val untpureTerm = cpsCtx.monad.unseal.select(pureSymbol)
          val tpureTerm = untpureTerm.appliedToType(otpe)
          val r = tpureTerm.appliedTo(origin)
          r
      

  abstract class AsyncCpsTree extends CpsTree:
                      
    def isAsync = true

    def transformed: qctx.tasty.Term

    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          MappedCpsTree(this,f, ntpe)

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          FlatMappedCpsTree(this,f, ntpe)



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
          val untmapTerm = cpsCtx.monad.unseal.select(mapSymbol)
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
          //   ${cpsCtx.monad}.map(${prev.transformed.seal.asInstanceOf[F[T]]})(
          //             (x:${prev.seal}) => ${op('x)}
          //   )
          //}.unseal
          r 
    }

      
  case class FlatMappedCpsTree(
                      val prev: CpsTree,
                      opm: Term => Term,
                      otpe: Type) extends AsyncCpsTree {

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
        val monad = cpsCtx.monad.unseal
        val untpFlatMapTerm = monad.select(flatMapSymbol)
        val tpFlatMapTerm = untpFlatMapTerm.appliedToTypes(List(prev.otpe,otpe))
        val r = tpFlatMapTerm.appliedToArgss(
            List(
              List(prev.transformed),
              List(
                Lambda(
                  MethodType(List("x"))(mt => List(prev.otpe), 
                                        mt => AppliedType(fType.unseal.tpe,List(otpe))),
                  opArgs => opm(opArgs.head.asInstanceOf[Term])
                )
             )
           )
        )
        r
    }


  }


}
