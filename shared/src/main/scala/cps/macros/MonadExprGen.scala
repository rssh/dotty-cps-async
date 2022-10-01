// CPS Transform expression building block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 20212022
package cps.macros

import scala.quoted.*
import scala.util.*

import cps.*
import cps.macros.misc.*

trait MonadExprGen[F[_]:Type]:

  type Context

  def pure[T](t:Expr[T])(using Quotes):Expr[F[T]]

  def map[A,B](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]]
  
  def flatMap[A,B](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]]

  def supportsTryCatch(using Quotes): Boolean

  def error[A](ex:Expr[Throwable])(using Quotes):Expr[F[A]]

  def mapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]]

  def flatMapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]]

  def restore[A](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] 

  def withAction[A](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]]   

  def contextType: Type[Context]

  def applyGen[T](op: Expr[Context =>F[T]])(using Quotes): Expr[F[T]]

  def adoptAwait[A](c:Expr[Context],fa:Expr[F[A]])(using Quotes):Expr[F[A]]

  def show: String

end MonadExprGen

object MonadExprGen:

   type Aux[F[_],C] = MonadExprGen[F] { type Context = C }

   def apply[F[_],C](dm:Expr[CpsContextCarrier.Aux[F,C]])(using Quotes): MonadExprGen[F] { type Context = C } = {
      import quotes.reflect.* 
      if (dm.asTerm.tpe <:< TypeRepr.of[CpsMonad[F]]) {
         CpsMonadExprGen[F,C](dm.asExprOf[CpsMonad[F]{type Context = C}])
      } else if (dm.asTerm.tpe <:< TypeRepr.of[InlineCpsMonad[F]] ) {
         InlineCpsMonadExprGen[F,C](dm.asExprOf[InlineCpsMonad[F]{ type Context = C} ])
      } else {
        throw MacroError(s"${dm.show} of type ${dm.asTerm.tpe.show} is not CpsMonad or CpsInlineMonad", dm)
      }
   }


end MonadExprGen


class CpsMonadExprGen[F[_]:Type,C<:CpsMonadContext[F]:Type](dm: Expr[CpsMonad[F] {type Context = C}]) extends MonadExprGen[F]:

  type Context = C 

  def pure[T](t:Expr[T])(using Quotes):Expr[F[T]] =
    '{  $dm.pure($t) }

  def map[A,B](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]] =
    '{ ${dm}.map($fa)($f) }

  def flatMap[A,B](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]] =
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

  def error[A](ex:Expr[Throwable])(using Quotes):Expr[F[A]] =
    '{ ${tryMonadExpr}.error($ex)  }   

  def mapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]] =
      '{ ${tryMonadExpr}.mapTry($fa)($f) }

  def flatMapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]] =
      '{ ${tryMonadExpr}.flatMapTry($fa)($f) }
  
  def restore[A](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] =
    '{ ${tryMonadExpr}.restore($fa)($fx) }

  def withAction[A](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]] =
    '{ ${tryMonadExpr}.withAction($fa)($action) }
  
  def applyGen[T](op: Expr[C =>F[T]])(using Quotes): Expr[F[T]] =
    '{ $dm.apply($op) }

  def adoptAwait[A](c:Expr[C],fa:Expr[F[A]])(using Quotes):Expr[F[A]] =
    '{ ${c}.adoptAwait($fa)  }

  def contextType: Type[Context] =
    summon[Type[C]]

end CpsMonadExprGen


class InlineCpsMonadExprGen[F[_]:Type,C:Type](dm: Expr[InlineCpsMonad[F] {type Context = C}]) extends MonadExprGen[F]:

  type Context = C 

  def pure[T](t:Expr[T])(using Quotes):Expr[F[T]] = {
    import quotes.reflect.*
    val pureSym = Select.unique(dm.asTerm,"pure")
    Apply(TypeApply(pureSym,List(Inferred(TypeRepr.of[T]))), List(t.asTerm) ).asExprOf[F[T]]
  }

  def map[A,B](fa:Expr[F[A]])(f: Expr[A=>B])(using Quotes): Expr[F[B]] = {
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
    

  def flatMap[A,B](fa:Expr[F[A]])(f: Expr[A=>F[B]])(using Quotes): Expr[F[B]] = {
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
    dm.asTerm.tpe <:< TypeRepr.of[InlineCpsTryMonad[F]]

  def tryMonadExpr(using Quotes): Expr[InlineCpsTryMonad[F]] = ???

  def error[A](ex:Expr[Throwable])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.*
    val errorSym = Select.unique(dm.asTerm, "error")
    Apply(
      TypeApply(errorSym,List(Inferred(TypeRepr.of[A]))),
      List(ex.asTerm)
    ).asExprOf[F[A]]
  }

  def mapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>B])(using Quotes): Expr[F[B]] = {
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


  def flatMapTry[A,B](fa: Expr[F[A]])(f: Expr[Try[A]=>F[B]])(using Quotes): Expr[F[B]] = {
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
  
  def restore[A](fa: Expr[F[A]])(fx: Expr[Throwable => F[A]])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.*
    //'{ ${tryMonadExpr}.restore($fa)($fx) }
    flatMapTry(fa)('{ x =>
      x match
        case Success(v) => ${pure('v)}
        case Failure(ex) => ${fx}.apply(ex)
    })
  }

  def withAction[A](fa: Expr[F[A]])(action: Expr[Unit])(using Quotes): Expr[F[A]] = {
    import quotes.reflect.*
    //'{ ${tryMonadExpr}.withAction($fa)($action) }
    flatMapTry(fa)('{ x =>
         ${action}
         x match
          case Success(v) => ${pure('v)}
          case Failure(e) => ${error('e)}
    }).asExprOf[F[A]]
  }
  
  def applyGen[T](op: Expr[C =>F[T]])(using Quotes): Expr[F[T]] =
    '{ $dm.apply($op) }

  def adoptAwait[A](c:Expr[C],fa:Expr[F[A]])(using Quotes):Expr[F[A]] =
  {  
    import quotes.reflect.*
    //'{ ${c}.adoptAwait(fa)  }
    // For now we think that in inline we have no operation.
    //TODO: rething.    
    fa
  }

  def contextType: Type[Context] =
    summon[Type[C]]

end InlineCpsMonadExprGen

