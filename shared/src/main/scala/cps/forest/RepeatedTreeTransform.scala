package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait RepeatedTreeTransform[F[_], CT]:

  thisScope: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  // case Repeated(elements,tpt) 
  def runRepeated( repeated: qctx.reflect.Term, 
                    elements: List[qctx.reflect.Term], 
                    tpt: qctx.reflect.TypeTree): CpsTree =
    val paramsDescriptor = new MethodParamsDescriptor {
       override def  paramIndex(name: String): Option[Int] = None
       override def  paramName(index: Int): Option[String] = Some(index.toString)
       override def  paramType(index: Int): Option[TypeRepr] = Some(tpt.tpe)
    }
    val args = O.buildApplyArgsRecords(paramsDescriptor, elements, cpsCtx)
    // TODO: pass allowShiftedLambda = false ?
    args.find(_.hasShiftedLambda).foreach(a =>
       throw MacroError("shifted lambda is not supported in SeqLiteral", posExprs(a.term))
    )
    if (!args.exists(x => x.isAsync)) then
       // TODO: check 'isChanged?'
       CpsTree.pure(repeated)
    else
       val syncArgs = args.map(_.identArg(true)).toList
       val rightCps = CpsTree.pure(Repeated(syncArgs, tpt))
       args.foldRight(rightCps)((e,s) =>
          if (e.usePrepend(true))
             e.append(s)
          else
             s
       )


object RepeatedTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         repeatedTerm: qctx1.reflect.Term, 
                         elements: List[qctx1.reflect.Term], 
                         tpt: qctx1.reflect.TypeTree): CpsExpr[F,T] = {
     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
          
         def bridge(): CpsExpr[F,T] =
            runRepeated(repeatedTerm.asInstanceOf[quotes.reflect.Term],
                         elements.asInstanceOf[List[quotes.reflect.Term]],
                         tpt.asInstanceOf[quotes.reflect.TypeTree]
                        ).toResult[T].asInstanceOf[CpsExpr[F,T]]

     } 
     (new Bridge(cpsCtx1)).bridge()
  }

