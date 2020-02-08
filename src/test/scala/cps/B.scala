package cps

import scala.quoted._
import scala.quoted.matching._


object T1 {

  def cbi(n:Int): ComputationBound[Int] = ???

  def cbBool(b:Boolean): ComputationBound[Boolean] = ???

}


object B {

 def await[T](e:ComputationBound[T]):T = ???

 inline def badTree[T](expr: =>T): ComputationBound[T] =
    ${ B.badTreeImpl[T]('expr) }

 def badTreeImpl[T](given qctx: QuoteContext)(input:Expr[T]):Expr[ComputationBound[T]] = {
  import qctx.tasty.{_,given}
  val q = input.unseal.underlyingArgument
  val yValDef = findValDef(q,"y")
  val yIdent = findIdent(q,"y")
  val t = 
    '{ComputationBoundAsyncMonad.flatMap[Boolean,Int]( T1.cbBool(true) )(
        (v:Boolean) =>
       ${
        Block(
         List(
           yValDef,
         ),
         '{
           ComputationBoundAsyncMonad.map[Int,Int](T1.cbi(2))( (v1: Int) => {
              ${yIdent.seal.asInstanceOf[Expr[Int]]} + v1
           })
         }.unseal
        ).seal.asInstanceOf[Expr[ComputationBound[Int]]]
       })
    }
  t.asInstanceOf[Expr[ComputationBound[T]]]
 }

 def findIdent(given qctx:QuoteContext)(code: qctx.tasty.Term, name: String):qctx.tasty.Term =
   import qctx.tasty.{_,given}
   find(code,{
     case ident@Ident(n) if n == name => Some(ident)
     case _ => None
   }).get.asInstanceOf[qctx.tasty.Ident]


 def findValDef(given qctx:QuoteContext)(code: qctx.tasty.Term, name: String):qctx.tasty.ValDef = {
   import qctx.tasty.{_,given}
   find(code,{
     case v@ValDef(n,tpt,rhs) if n == name => Some(v)
     case _ => None
   }).get.asInstanceOf[qctx.tasty.ValDef]
 }

  def find(given qctx:QuoteContext)(term: qctx.tasty.Tree,
                       cond: qctx.tasty.Tree=> Option[qctx.tasty.Tree]):Option[qctx.tasty.Tree] = {
     import qctx.tasty.{_,given}
     import util._
     val search = new TreeAccumulator[Option[Tree]] {

        def foldTree(x: Option[Tree], tree: Tree)(given ctx: Context): Option[Tree] =
                 foldOverTree(x,tree)

        override def foldOverTree(x: Option[Tree], tree: Tree)(given ctx: Context): Option[Tree] = {
           if (x.isDefined)
             x
           else
             cond(tree) orElse
                super.foldOverTree(x,tree)
        }
     }
     search.foldTree(None,term)
  } 
 

}
