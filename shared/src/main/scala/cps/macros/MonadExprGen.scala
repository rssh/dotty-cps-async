// CPS Transform expression building block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 20212022
package cps.macros

import scala.quoted.*
import scala.util.*

import cps.*
import cps.macros.misc.*

trait MonadExprGen[F[_]:Type]:

  type Context

  type CpsMonadInstance

  def pure[T:Type](t:Expr[T])(using Quotes):Expr[F[T]]

  def map[A:Type,B:Type](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]]
  
  def flatMap[A:Type,B:Type](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]]

  def supportsTryCatch(using Quotes): Boolean

  def error[A:Type](ex:Expr[Throwable])(using Quotes):Expr[F[A]]

  def mapTry[A:Type,B:Type](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]]

  def flatMapTry[A:Type,B:Type](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]]

  def restore[A:Type](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] 

  def withAction[A:Type](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]]   

  def withAsyncAction[A:Type](fa:Expr[F[A]])(action: Expr[F[Unit]])(using Quotes): Expr[F[A]] 

  def tryImpure[A:Type](a:Expr[F[A]])(using Quotes):Expr[F[A]]

  def tryPure[A:Type](a:Expr[A])(using Quotes):Expr[F[A]]

  def contextType: Type[Context]

  //def applyGen[T:Type](op: Expr[Context =>F[T]])(using Quotes): Expr[F[T]]

  def adoptAwait[A:Type](c:Expr[Context],fa:Expr[F[A]])(using Quotes):Expr[F[A]]

  def monadInstance(using Quotes): Expr[CpsMonadInstance]

  def show(using Quotes): String

end MonadExprGen

object MonadExprGen:

   type Aux[F[_],C] = MonadExprGen[F] { type Context = C }

   def apply[F[_]:Type,C <: CpsMonadContext[F]:Type](dm:Expr[CpsContextCarrier[F]])(using Quotes): MonadExprGen[F] { type Context = C } = {
      import quotes.reflect.* 
      if (dm.asTerm.tpe <:< TypeRepr.of[CpsMonad[F]]) {
         dm.asTerm.tpe.widen.asType match
          case '[t] =>
              CpsMonadExprGen[F,C,t & CpsMonad[F]](dm.asExprOf[t & CpsMonad[F]] )
          case _ =>
              throw MacroError(s"Can't get type for ${dm.show}", dm)     
      } else if (dm.asTerm.tpe <:< TypeRepr.of[CpsInlineMonad[F]] ) {
        dm.asTerm.tpe.widen.asType match
          case '[t] =>         
            InlineCpsMonadExprGen[F,C, t & CpsInlineMonad[F]](dm.asExprOf[t & CpsInlineMonad[F]])
      } else {
        throw MacroError(s"${dm.show} of type ${dm.asTerm.tpe.show} is not CpsMonad or CpsInlineMonad", dm)
      }
   }


end MonadExprGen


class CpsMonadExprGen[F[_]:Type,C<:CpsMonadContext[F]:Type,M <: CpsMonad[F]:Type](dm: Expr[M]) extends MonadExprGen[F]:

  type Context = C 

  type CpsMonadInstance = M

  override def pure[T:Type](t:Expr[T])(using Quotes):Expr[F[T]] =
    '{  $dm.pure($t) }

  def map[A:Type,B:Type](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]] =
    '{ ${dm}.map($fa)($f) }

  def flatMap[A:Type,B:Type](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]] =
    '{ ${dm}.flatMap($fa)($f) }

  def supportsTryCatch(using Quotes): Boolean =
    import quotes.reflect.*
    dm.asTerm.tpe <:< TypeRepr.of[CpsTryMonad[F]]

  def tryMonadExpr(using Quotes): Expr[CpsTryMonad[F]] =
    import quotes.reflect.*
    if (dm.asTerm.tpe <:< TypeRepr.of[CpsTryMonad[F]]) {
       dm.asExprOf[CpsTryMonad[F]]
    } else {
       report.throwError("Monad is not supports try/catch")
    } 

  def error[A:Type](ex:Expr[Throwable])(using Quotes):Expr[F[A]] =
    '{ ${tryMonadExpr}.error($ex)  }   

  def mapTry[A:Type,B:Type](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]] =
      '{ ${tryMonadExpr}.mapTry($fa)($f) }

  def flatMapTry[A:Type,B:Type](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]] =
      '{ ${tryMonadExpr}.flatMapTry($fa)($f) }
  
  def restore[A:Type](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] =
    '{ ${tryMonadExpr}.restore($fa)($fx) }

  def withAction[A:Type](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]] =
    '{ ${tryMonadExpr}.withAction($fa)($action) }

  def withAsyncAction[A:Type](fa:Expr[F[A]])(action: Expr[F[Unit]])(using Quotes): Expr[F[A]] =
    '{ ${tryMonadExpr}.withAsyncAction($fa)($action) }

  def tryImpure[A:Type](a:Expr[F[A]])(using Quotes):Expr[F[A]] =
      '{ ${tryMonadExpr}.tryImpure($a) }
  
  def tryPure[A:Type](a:Expr[A])(using Quotes):Expr[F[A]] =
      '{ ${tryMonadExpr}.tryPure($a) }
  
  //def applyGen[T:Type](op: Expr[C =>F[T]])(using Quotes): Expr[F[T]] =
  //  '{ $dm.apply($op) }

  def adoptAwait[A:Type](c:Expr[C],fa:Expr[F[A]])(using Quotes):Expr[F[A]] =
    '{ ${c}.adoptAwait($fa)  }

  def contextType: Type[Context] =
    summon[Type[C]]

  def monadInstance(using Quotes): Expr[CpsMonadInstance] =
    dm

  def show(using Quotes): String =
    dm.show


end CpsMonadExprGen


class InlineCpsMonadExprGen[F[_]:Type,C:Type,M <: CpsInlineMonad[F]:Type ](dm: Expr[M]) extends MonadExprGen[F]:

  type Context = C 

  type CpsMonadInstance = M

  def pure[T:Type](t:Expr[T])(using Quotes):Expr[F[T]] = {
    import quotes.reflect.*
    val pureSym = Select.unique(dm.asTerm,"pure")
    Apply(TypeApply(pureSym,List(Inferred(TypeRepr.of[T]))), List(t.asTerm) ).asExprOf[F[T]]
  }

  def map[A:Type,B:Type](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]] = {
    import quotes.reflect.*
    //'{ ${dm}.map($fa)($f) }
    val mapSym = Select.unique(dm.asTerm,"map")
    Apply(
      Apply(
        TypeApply(
          mapSym,
          List(
            Inferred(TypeRepr.of[A]),
            Inferred(TypeRepr.of[B])
          )
        ),
        List(fa.asTerm)
      ),
      List(f.asTerm)
    ).asExprOf[F[B]]
  }
    

  def flatMap[A:Type,B:Type](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]] = {
    import quotes.reflect.*
    //'{ ${dm}.flatMap($fa)($f) }
    val flatMapSym = Select.unique(dm.asTerm,"flatMap")
    Apply(
      Apply(
        TypeApply(
          flatMapSym,
          List(
            Inferred(TypeRepr.of[A]),
            Inferred(TypeRepr.of[B])
          )
        ),
        List(fa.asTerm)
      ),
      List(f.asTerm)
    ).asExprOf[F[B]]
  }

  def supportsTryCatch(using Quotes): Boolean =
    import quotes.reflect.*
    dm.asTerm.tpe <:< TypeRepr.of[CpsInlineTryMonad[F]]

  def tryMonadExpr(using Quotes): Expr[CpsInlineTryMonad[F]] = ???

  def error[A:Type](ex:Expr[Throwable])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.*
    val errorSym = Select.unique(dm.asTerm, "error")
    Apply(
      TypeApply(errorSym,List(Inferred(TypeRepr.of[A]))),
      List(ex.asTerm)
    ).asExprOf[F[A]]
  }

  def mapTry[A:Type,B:Type](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]] = {
    import quotes.reflect.*
    // '{ ${tryMonadExpr}.mapTry($fa)($f) }
    val mapTrySym = Select.unique(dm.asTerm,"mapTry")
    Apply(
      Apply(
        TypeApply(
          mapTrySym,
          List(
            Inferred(TypeRepr.of[A]),
            Inferred(TypeRepr.of[B])
          )
        ),
        List(fa.asTerm)
      ),
      List(f.asTerm)
    ).asExprOf[F[B]]
  }


  def flatMapTry[A:Type,B:Type](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]] = {
    import quotes.reflect.*
    // '{ ${tryMonadExpr}.flatMapTry($fa)($f) }
    val flatMapTrySym = Select.unique(dm.asTerm,"flatMapTry")
    Apply(
      Apply(
        TypeApply(
          flatMapTrySym,
          List(
            Inferred(TypeRepr.of[A]),
            Inferred(TypeRepr.of[B])
          )
        ),
        List(fa.asTerm)
      ),
      List(f.asTerm)
    ).asExprOf[F[B]]
  }
  
  def restore[A:Type](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.*
    //'{ ${tryMonadExpr}.restore($fa)($fx) }
    flatMapTry(fa)('{ x =>
      x match
        case Success(v) => ${pure('v)}
        case Failure(ex) => ${fx}.apply(ex)
    })
  }

  def withAction[A:Type](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.*
    // '{ ${tryMonadExpr}.withAction(fa)(action) }
    Apply(
      Apply(
        TypeApply(
           Select.unique(dm.asTerm,"withAction"),
           List(Inferred(TypeRepr.of[A])) 
        ),
        List(fa.asTerm)
      ),
      List(action.asTerm)
    ).asExprOf[F[A]]
  }

  def withAsyncAction[A:Type](fa: Expr[F[A]])(action: Expr[F[Unit]])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.*
    // '{ ${tryMonadExpr}.withAsyncAction(fa)(action) }
    Apply(
      Apply(
        TypeApply(
           Select.unique(dm.asTerm,"withAsyncAction"),
           List(Inferred(TypeRepr.of[A])) 
        ),
        List(fa.asTerm)
      ),
      List(action.asTerm)
    ).asExprOf[F[A]]
  }
  
  def tryImpure[A:Type](a:Expr[F[A]])(using Quotes):Expr[F[A]] = {
    import quotes.reflect.*
    val funSym = Select.unique(dm.asTerm,"tryImpure")
    Apply(
      TypeApply(funSym, List(Inferred(TypeRepr.of[A]))),
      List(a.asTerm)
    ).asExprOf[F[A]]
  }

  def tryPure[A:Type](a:Expr[A])(using Quotes):Expr[F[A]] = {
    import quotes.reflect.*
    val funSym = Select.unique(dm.asTerm,"tryPure")
    Apply(
      TypeApply(funSym, List(Inferred(TypeRepr.of[A]))),
      List(a.asTerm)
    ).asExprOf[F[A]]
  }


  //def applyGen[T:Type](op: Expr[C =>F[T]])(using Quotes): Expr[F[T]] =
  //  '{ $dm.apply($op) }

  def adoptAwait[A:Type](c:Expr[C],fa:Expr[F[A]])(using Quotes):Expr[F[A]] =
  {  
    import quotes.reflect.*
    //'{ ${c}.adoptAwait(fa)  }
    // For now we think that in inline we have no operation.
    //TODO: rething.    
    fa
  }

  def contextType: Type[Context] =
    summon[Type[C]]

  def monadInstance(using Quotes): Expr[CpsMonadInstance] =
    dm
  
  def show(using Quotes): String =
    dm.show
    

end InlineCpsMonadExprGen

