package cps

import scala.quoted._

case class TransformationContext[F[_],T](
   patternCode: Expr[T],  // code, for which we build pattern expression
   patternType: Type[T],
   monad: Expr[CpsMonad[F]],
   memoization: Option[TransformationContext.Memoization[F]],
   flags: AsyncMacroFlags,
   marker: TransformationContextMarker,
   nesting: Int,
   parent: Option[TransformationContext[_,_]],
)  {

  def nestSame(marker: TransformationContextMarker, muted: Boolean = flags.muted): TransformationContext[F,T] = 
           copy(marker=marker, nesting=nesting+1, parent=Some(this))

  def nest[S](newPatternCode: Expr[S], newPatternType: Type[S], marker: TransformationContextMarker, 
                                                  muted: Boolean = flags.muted): 
             TransformationContext[F,S] =
      TransformationContext(newPatternCode, newPatternType, monad, memoization, flags.copy(muted=muted), 
                             marker, nesting + 1, parent=Some(this) )

  def log(message:String): Unit =
       if !flags.muted then
         print("  "*nesting)   
         println(message)   


}

object TransformationContext {

  case class Memoization[F[_]](
      kind: MonadMemoizationKind,
      monadMemoization: Expr[CpsMonadMemoization[F]]
  )

}
