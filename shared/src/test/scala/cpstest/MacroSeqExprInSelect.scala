package cpstest

import scala.quoted.*
import scala.reflect.ClassTag



object MacroSeqExprInSelect {

  class RefCollection(array: Array[Ref[?]]) {
    def get[T](i: Int): T = array(i).value.asInstanceOf[T]
  }

  case class Ref[T](value: T)

  inline def initArrayExpr1[T](expr1: T): RefCollection = ${
    initAsArrayExpr1Impl('expr1)
  }

  inline def initArrayExpr2[T1,T2](expr1: T1, expr2: T2): RefCollection= ${
    initAsArrayExpr2Impl('expr1, 'expr2)
  }

  def initAsArrayExpr1Impl[T:Type](expr1: Expr[T])(using Quotes): Expr[RefCollection] = {
    initAsArrayExprImpl(Seq(expr1))
  }

  def initAsArrayExpr2Impl[T1:Type,T2:Type](expr1: Expr[T1], expr2: Expr[T2])(using Quotes): Expr[RefCollection] = {
    initAsArrayExprImpl(Seq(expr1, expr2))
  }


  def initAsArrayExprImpl(exprs: Seq[Expr[Any]])(using Quotes): Expr[RefCollection] = {
    '{ new RefCollection( ${Expr.ofSeq(exprs.map(e => '{Ref($e)})) }.toArray) }
  }


}
