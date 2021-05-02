/**
 * part of dotty-cps-async
 * (C) Ruslan Shevchenko, <ruslan@shevchenko.kiev.ua>, 2020, 2021
 **/
package cps.misc

import scala.quoted._

object WithOptExprProxy:

  def apply[T:Type,S:Type](name: String, originExpr: Expr[T])(buildExpr: Expr[T] => Expr[S])(using Quotes): Expr[S] =
    import quotes.reflect.*
    originExpr.asTerm match
       case Ident(_) => buildExpr(originExpr) 
       case originTerm =>
         val proxy = Symbol.newVal(Symbol.spliceOwner,name,originExpr.asTerm.tpe.widen,Flags.EmptyFlags,Symbol.noSymbol)
         val proxyValDef = ValDef(proxy,Some(originTerm))
         val proxyIdent = Ref(proxy).asExprOf[T]
         val resTerm = buildExpr(proxyIdent).asTerm match
           case Block(stats, expr) => Block(proxyValDef::stats, expr)
           case expr => Block(List(proxyValDef), expr)
         resTerm.asExprOf[S]
     
    


  

