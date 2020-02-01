package cps.forest

import scala.quoted._
import cps._

trait CpsTreeScope[F[_]] {

  cpsTreeScope: TreeTransformScope[F] =>

  import qctx.tasty.{_,given}

  sealed abstract class CpsTree

     def isAsync: Boolean

     def transformed: Term

     def typeApply(targs: List[qctx.tasty.TypeTree], ntpe: Type): CpsTree =
           applyTerm(_.appliedToTypeTrees(targs), ntpe)

     def applyTerm(f: Term => Term, ntpe: Type): CpsTree

     def monadMap(f: Term => Term, ntpe:Type): CpsTree

     def monadFlatMap(f: Term => Term, ntpe:Type): CpsTree

     def otpe: Type

     def toResult[T: quoted.Type](origin: Expr[T]) : CpsExpr[F,T] =
       import cpsCtx._
       this match
         case syncTerm: PureCpsTree =>
             val code = origin
             CpsExpr.sync(asyncMonad,code)
         case cpsTree: AsyncCpsTree =>
             val transformed = cpsTree.transformed.seal.asInstanceOf[Expr[F[T]]]
             CpsExpr.async[F,T](asyncMonad, transformed)

  object CpsTree

    def pure(origin:Term): CpsTree = PureCpsTree(origin)

    def impure(transformed:Term, tpe: Type): CpsTree = 
                   AwaitCpsTree(transformed, tpe)

  

  case class PureCpsTree(origin: qctx.tasty.Term) extends CpsTree

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

    def transformed: Term = TransformUtil.makePure(cpsCtx.asyncMonad, origin)
       

  abstract class AsyncCpsTree extends CpsTree
                      
    def isAsync = true

    def transformed: qctx.tasty.Term

    def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          MappedCpsTree(this,f, ntpe)

    def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          FlatMappedCpsTree(this,f, ntpe)



  class AwaitCpsTree(val transformed: Term, val otpe: Type) extends AsyncCpsTree
                     
    def applyTerm(f: Term => Term, ntpe: Type): CpsTree =
          AwaitCpsTree(f(transformed), ntpe)


  case class MappedCpsTree(prev: CpsTree, op: Term => Term, otpe: Type) extends AsyncCpsTree

    def applyTerm(f: Term => Term, npte: Type): CpsTree =
          MappedCpsTree(prev, t => f(op(t)), npte)

    override def monadMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.map(f) = prev.map(op).map(f) = prev.map(op*f)
          MappedCpsTree(prev, t => f(op(t)), ntpe)

    override def monadFlatMap(f: Term => Term, ntpe: Type): CpsTree =
          // this.flatMap(f) = prev.map(op).flatMap(f) = prev.flatMap(op*f)
          FlatMappedCpsTree(prev, t => f(op(t)), ntpe)

    def transformed: Term = ???

      
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

    def transformed: Term = ???


  }
                     

}
