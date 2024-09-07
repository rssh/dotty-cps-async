package cps.macros


import scala.quoted.*

import cps.*

case class TransformationContext[F[_]:Type,T,C <: CpsMonadContext[F]](
   patternCode: Expr[T],  // code, for which we build pattern expression
   patternType: Type[T],
   monad: Expr[CpsMonad[F]],
   monadContext: Expr[C],
   runtimeAwait: Option[Expr[CpsRuntimeAwait[F]]],
   runtimeAwaitProvider: Option[Expr[CpsRuntimeAwaitProvider[F]]],
   flags: AsyncMacroFlags,
   nesting: Int,
   parent: Option[TransformationContext[?,?,?]],
)  {

  //def monad(using Quotes): Expr[CpsMonad[F]] =
  //  import quotes.reflect.*
  //  Select.unique(monadContext.asTerm, "monad").asExprOf[CpsMonad[F]]

  def tryMonad(using Quotes): Expr[CpsTryMonad[F]] =
    import quotes.reflect.*
    Select.unique(monadContext.asTerm, "monad").asExprOf[CpsTryMonad[F]]
    // idea for other branch: let context will provide us tryMonad
    //Select.unique(monadContext.asTerm, "tryMonad").asExprOf[CpsTryMonad[F]]

  def nestSame(muted: Boolean = flags.muted): TransformationContext[F,T,C] = 
           copy(flags = flags.copy(muted = muted), nesting=nesting+1, parent=Some(this))

  def nest[S](newPatternCode: Expr[S], newPatternType: Type[S], 
                                         muted: Boolean = flags.muted):   TransformationContext[F,S,C] =
      TransformationContext(newPatternCode, newPatternType, monad, monadContext,
                             runtimeAwait, runtimeAwaitProvider,
                             flags.copy(muted=muted),
                             nesting + 1, parent=Some(this) )

  def log(message:String): Unit =
       if !flags.muted then
         print("  "*nesting)   
         println(message)   


}


object TransformationContext {

  /*
  case class Memoization[F[_]](
      kind: CpsMonadMemoization.Kind,
      monadMemoization: Expr[CpsMonadMemoization[F]]
  )

   */

}
