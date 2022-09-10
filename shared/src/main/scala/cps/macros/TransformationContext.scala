package cps.macros


import scala.quoted.*

import cps.*
import cps.macros.observatory.*

case class TransformationContext[F[_],T,C <: CpsMonadContext[F]](
   patternCode: Expr[T],  // code, for which we build pattern expression
   patternType: Type[T],
   monad: Expr[CpsMonad[F]],
   monadContext: Expr[C],
   memoization: Option[TransformationContext.Memoization[F]],
   runtimeAwait: Option[Expr[CpsRuntimeAwait[F]]],
   flags: AsyncMacroFlags,
   observatory: Observatory.Scope#Observatory,
   nesting: Int,
   parent: Option[TransformationContext[_,_,_]],
)  {

  def nestSame(muted: Boolean = flags.muted): TransformationContext[F,T,C] = 
           copy(flags = flags.copy(muted = muted), nesting=nesting+1, parent=Some(this))

  def nest[S](newPatternCode: Expr[S], newPatternType: Type[S], 
                                         muted: Boolean = flags.muted):   TransformationContext[F,S,C] =
      TransformationContext(newPatternCode, newPatternType, monad, monadContext, memoization, runtimeAwait,
                             flags.copy(muted=muted), 
                             observatory, nesting + 1, parent=Some(this) )

  def log(message:String): Unit =
       if !flags.muted then
         print("  "*nesting)   
         println(message)   


}

object TransformationContext {

  case class Memoization[F[_]](
      kind: CpsMonadMemoization.Kind,
      monadMemoization: Expr[CpsMonadMemoization[F]]
  )

}
