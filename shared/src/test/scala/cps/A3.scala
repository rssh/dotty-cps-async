package cps

import scala.quoted._


trait X[T] { type V = T }

trait MM[F[_]] {

  def mmTransform[T:Type](expr: Expr[T])(using Quotes):Expr[F[T]]

}

implicit object MMX extends MM[X] {

  def mmTransform[T:Type](expr: Expr[T])(using Quotes):Expr[X[T]] =
   '{ new X{ val v=${expr} } }

}

object A3 {

  inline def a[F[_],T](x: =>T):F[T] = 
     ${ A3.aTransform[F,T]('x) }
  

  def aTransform[F[_]:Type,T:Type](e:Expr[T])(using Quotes):Expr[F[T]] = 
     // call mmTransform here
     Expr.summon[MM[F]] match 
         case Some(mme) =>  // we have Expr[MM[F]] here, need MM[F]
                         val mm: MM[F] = ???  //   staging not works inside macros.
                         mm.mmTransform(e) 
         case None => 
                      val ft = summon[Type[F]]
                      val msg = s"MM not found for ${ft}"
                      throw new RuntimeException(msg)
                      //compiletime.error(msg)

}

  
