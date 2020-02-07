package cps

import scala.quoted._
import scala.quoted.matching._

trait CB[T]

object CBM {

   def pure[T](value:T): CB[T] = ???

}

object B {

  inline def badTree[T](q: =>CB[T]): CB[T] =
    ${ badTreeImpl('q) }

  def badTreeImpl[T:Type](qe:Expr[CB[T]])(given qctx: QuoteContext): Expr[CB[T]] = {
    import qctx.tasty.{_,given}
    val q = qe.unseal.underlyingArgument
    val x2Constant = Literal(Constant(4))
    val plusSelect = extractSelect("x1",q)
    val lastExpr3 = Apply(plusSelect, List(x2Constant)).seal.asInstanceOf[Expr[T]]
    val r3= '{ CBM.pure($lastExpr3) }
    val oldValDef0 = extractValDefExpr("x1",q.seal).get
    val r0=Block(List(oldValDef0), r3.unseal).seal.asInstanceOf[Expr[CB[T]]]
    r0
  }

  def extractValDefExpr(given qctx:QuoteContext)(name:String, code: Expr[Any]): Option[qctx.tasty.ValDef] =
   import qctx.tasty.{_,given}
   code.unseal match {
     case Block(stats, last) =>
       stats.head match {
         case v@ValDef(vName,_,_) if (vName == name) => Some(v)
         case other => if (stats.tail == Nil) 
                          None 
                       else 
                          extractValDefExpr(name, Block(stats.tail, last).seal)
       }
     case Inlined(call,binding,body) =>
                          extractValDefExpr(name, body.seal)
     case _ => 
        println(s"Can't extract valDef: code=${code.unseal}")
        throw new IllegalStateException("AAA")
   }

  def extractValDef(given qctx:QuoteContext)(name:String, code: qctx.tasty.Tree): qctx.tasty.ValDef =
   import qctx.tasty.{_,given}
   find(code,{
     case v@ValDef(n,_,_) if n == name => Some(v)
     case _ => None
   }).get.asInstanceOf[qctx.tasty.ValDef]

  def extractSelect(given qctx:QuoteContext)(name:String, code: qctx.tasty.Term): qctx.tasty.Term =
   import qctx.tasty.{_,given}
   find(code,{
     case s@Select(Ident(n),_) if n == name => Some(s)
     case _ => None
   }).get.asInstanceOf[qctx.tasty.Term]

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
