package cps.macros.forest

import cps._
import cps.macros._
import cps.macros.forest.application._
import scala.quoted._

trait TreeTransformScope[F[_]:Type,CT:Type, CC<:CpsMonadContext[F]:Type]
               extends CpsTreeScope[F, CT, CC]
                  with KnownTreeFragments[F,CT, CC]
                  with TypeApplyTreeTransform[F,CT, CC]
                  with RootTreeTransform[F, CT, CC]
                  with ApplyTreeTransform[F,CT, CC]
                  with ApplicationHelper[F,CT, CC]
                  with AwaitTreeTransform[F, CT, CC]
                  with SelectTreeTransform[F, CT, CC]
                  with LambdaTreeTransform[F, CT, CC]
                  with MatchTreeTransform[F, CT, CC]
                  with AsyncTreeShifter[F,CT, CC]
                  with RepeatedTreeTransform[F,CT, CC]
                  with InlinedTreeTransform[F,CT, CC]
                  with SelectOuterTreeTransform[F,CT, CC]
                  with BlockTreeTransform[F,CT,CC]
                  with ValDefTreeTransform[F,CT,CC]
{

   val cpsCtx: TransformationContext[F,CT, CC]

   implicit val qctx: Quotes

   implicit val fType: quoted.Type[F]

   implicit val ctType: quoted.Type[CT]

   implicit val ccType: quoted.Type[CC]

   def unitTerm = {
      import qctx.reflect.*
      Literal(UnitConstant())
   }

   def posExpr(t: qctx.reflect.Term): Expr[Any] =
       import qctx.reflect._
       t.tpe.widen match
         case MethodType(_,_,_) | PolyType(_,_,_) =>
           val etaExpanded = t.etaExpand(Symbol.spliceOwner)
           try
             etaExpanded.asExpr
           catch
             case ex: Exception =>
                // TODO: via reporting
                // println(s"etaExpanding not help, t.tpe.widen=${t.tpe.widen}")
                //ex.printStackTrace
                cpsCtx.patternCode
         case _ => t.asExpr

   def posExprs(terms: qctx.reflect.Term*): Expr[Any] =
       import qctx.reflect._
       var rest = terms
       var retval: Option[Expr[Any]] = None
       while(!retval.isDefined && !rest.isEmpty) 
         val t = rest.head
         rest = rest.tail
         t.tpe.widen match
           case MethodType(_,_,_) | PolyType(_,_,_) =>
              val etaExpanded = t.etaExpand(Symbol.spliceOwner)
              try
                retval = Some(etaExpanded.asExpr)
              catch
                case ex: Exception =>
                   //do nothing
           case _ => retval = Some(t.asExpr)
       retval.getOrElse(cpsCtx.patternCode)

   def safeShow(t: qctx.reflect.Tree): String =
       import qctx.reflect._
       try 
         t.show
       catch 
         case ex: Exception =>
            t.toString

   def safeTypeShow(tp: qctx.reflect.TypeRepr): String =
        import qctx.reflect._
        try {
          tp.show
        }catch
          case ex: Exception =>
            tp.toString 


   case class MessageWithPos(message:String, pos: qctx.reflect.Position)

   def isInMonad(tpe: qctx.reflect.TypeRepr): Boolean =
      given Type[F] = fType
      tpe.widen.asType match
        case '[F[r]] => true
        case _  => false


}


trait TreeTransformScopeInstance[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](
         override val cpsCtx: TransformationContext[F,T,C])
         (implicit override val qctx: Quotes)
                                 extends TreeTransformScope[F,T, C] {


}

