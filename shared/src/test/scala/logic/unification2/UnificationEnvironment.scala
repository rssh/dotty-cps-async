package logic.unification2

import cps.*
import logic.*
import logic.CpsLogicMonad


trait UnificationEnvironment[M[_]] extends CpsLogicMonad[M] {

  override type Context <: UnificationEnvironmentContext[M]

  def toTerm[A:Unifiable](ma: M[A]): M[TypedLogicalTerm[A]]

  def fromTerm[A:Unifiable](t:TypedLogicalTerm[A]): M[A]

  def bindTerm[A:Unifiable](v:LogicalVariable[A], t:TypedLogicalTerm[A]): M[A]

  def bind[A:Unifiable](v:LogicalVariable[A], ma: M[A]): M[A] 
  
}

trait UnificationEnvironmentContext[M[_]] extends CpsLogicMonadContext[M] {

  override def monad: UnificationEnvironment[M]

}

extension [M[_],A](ma:M[A])(using env: UnificationEnvironmentContext[M], ua: Unifiable[A])  {

  def _toTerm: M[TypedLogicalTerm[A]] =
    env.monad.toTerm(ma)

  transparent inline def toTerm: TypedLogicalTerm[A] =
    reflect(env.monad.toTerm(ma))
  
}

extension [M[_],A](lv:LogicalVariable[A])(using env: UnificationEnvironmentContext[M], ua: Unifiable[A])  {

  def _bindTerm(t:TypedLogicalTerm[A]): M[A] =
    env.monad.bindTerm(lv, t)

  transparent inline def bindTerm(t:TypedLogicalTerm[A]): A =
    reflect(env.monad.bindTerm(lv, t))
    
  def _bind(ma:M[A]): M[A] =
    env.monad.bind(lv, ma)
    
  transparent inline def bind(ma:M[A]): A =
    reflect(env.monad.bind(lv, ma))  

}

extension [M[_],A](t:TypedLogicalTerm[A])(using ua: Unifiable[A], ctx: UnificationEnvironmentContext[M])  {

  def _fromTerm: M[A] =
    ctx.monad.fromTerm(t)

  transparent inline def fromTerm: A =
    reflect(ctx.monad.fromTerm(t))

  
}


extension [M[_]](t: LogicalTerm)(using ctx: UnificationEnvironmentContext[M], env: UnificationEnvironment[M])  {


  def _castToTerm[S: Unifiable]: M[TypedLogicalTerm[S]] =
    t.symbol.castTo[M, S](t)

  transparent inline def castToTerm[S: Unifiable]: TypedLogicalTerm[S] =
    reflect(t.symbol.castTo[M, S](t))

  def castToM[S: Unifiable]: M[S] =
    ctx.monad.flatMap(t.symbol.castTo[M, S](t))(ctx.monad.fromTerm(_))


}


transparent inline def failure[M[_],A](using env: UnificationEnvironmentContext[M]): A =
  reflect(env.monad.mzero[A])
  
transparent inline def success[M[_],A](a:A)(using env: UnificationEnvironmentContext[M]): A =
  reflect(env.monad.pure(a))



object UnificationEnvironment {




}
