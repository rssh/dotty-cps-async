package cps.forest

import cps._
import cps.forest.application._
import scala.quoted._

trait TreeTransformScope[F[_]:Type,CT:Type]
               extends CpsTreeScope[F, CT]
                  with KnownTreeFragments[F,CT]
                  with TypeApplyTreeTransform[F,CT]
                  with RootTreeTransform[F, CT]
                  with ApplyTreeTransform[F,CT]
                  with ApplicationHelper[F,CT]
                  with AwaitTreeTransform[F, CT]
                  with SelectTreeTransform[F, CT]
                  with LambdaTreeTransform[F, CT]
                  with MatchTreeTransform[F, CT]
                  with AsyncTreeShifter[F,CT]
{

   val cpsCtx: TransformationContext[F,CT]

   implicit val qctx: QuoteContext

   implicit val fType: quoted.Type[F]

   implicit val ctType: quoted.Type[CT]

   def posExpr(t: qctx.reflect.Term): Expr[Any] =
       import qctx.reflect._
       t.tpe.widen match
         case MethodType(_,_,_) | PolyType(_,_,_) =>
           val etaExpanded = t.etaExpand
           try
             etaExpanded.seal
           catch
             case ex: Exception =>
                // TODO: via reporting
                // println(s"etaExpanding not help, t.tpe.widen=${t.tpe.widen}")
                //ex.printStackTrace
                cpsCtx.patternCode
         case _ => t.seal

   def posExprs(terms: qctx.reflect.Term*): Expr[Any] =
       import qctx.reflect._
       var rest = terms
       var retval: Option[Expr[Any]] = None
       while(!retval.isDefined && !rest.isEmpty) 
         val t = rest.head
         rest = rest.tail
         t.tpe.widen match
           case MethodType(_,_,_) | PolyType(_,_,_) =>
              val etaExpanded = t.etaExpand
              try
                retval = Some(etaExpanded.seal)
              catch
                case ex: Exception =>
                   //do nothing
           case _ => retval = Some(t.seal)
       retval.getOrElse(cpsCtx.patternCode)
         


   def safeShow(t: qctx.reflect.Term): String =
       import qctx.reflect._
       try 
         t.seal.show
       catch 
         case ex: Exception =>
            t.toString

}


trait TreeTransformScopeInstance[F[_]:Type,T:Type](
         override val cpsCtx: TransformationContext[F,T])
         (implicit override val qctx: QuoteContext)
                                 extends TreeTransformScope[F,T] {


}

