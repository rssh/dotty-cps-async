package cps.forest

import scala.quoted._

import cps._
import cps.misc._


trait RootTreeTransform[F[_], CT]:

  thisTransform: TreeTransformScope[F, CT] =>
  
  import qctx.reflect._

  def runRoot(term: qctx.reflect.Term, marker: TransformationContextMarker, muted: Boolean = false): CpsTree =
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRoot: term=$safeShow(term)")
     val r = term.tpe.widen match {
       case _ : MethodType =>
               //  in such case, we can't transform tree to expr
               //  without eta-expansion.  
               //    from other side - we don't want do eta-expand now, it can be performed early.
                runRootUneta(term, marker, muted)
       case _ : PolyType =>
                runRootUneta(term, marker, muted)
       case _ =>
                term match
                  case lambdaTerm@Lambda(params, body) => 
                                 // type of cps[x => y]  is  x=>F[y], not F[X=>Y]
                                 //  and if it violate CpsExpr contract (which require F[X=>Y]), let's
                                 //  work with lambda on the tree level.
                            B2.inNestedContext(lambdaTerm, marker, muted, scope =>
                                 scope.runLambda(lambdaTerm.asInstanceOf[scope.qctx.reflect.Term], 
                                                 params.asInstanceOf[List[scope.qctx.reflect.ValDef]], 
                                                 body.asInstanceOf[scope.qctx.reflect.Term]).inCake(thisTransform)
                            )
                  case applyTerm@Apply(fun,args)  =>
                            val tree = B2.inNestedContext(applyTerm, marker, muted, scope =>
                               scope.runApply(applyTerm.asInstanceOf[scope.qctx.reflect.Term],
                                              fun.asInstanceOf[scope.qctx.reflect.Term],
                                              args.asInstanceOf[List[scope.qctx.reflect.Term]],
                                              Nil).inCake(thisTransform)
                            )
                            tree.inCake(thisTransform)
                  case _ =>  // TODO: elimi
                    val expr = term.asExpr
                    val monad = cpsCtx.monad
                    expr match 
                      case '{ $e: et } =>
                        val rCpsExpr = try {
                             val nFlags = cpsCtx.flags.copy(muted = muted || cpsCtx.flags.muted)
                             Async.nestTransform(e, cpsCtx.copy(flags = nFlags), marker)
                        } catch {
                             case e: Throwable =>
                                println(s"can't translate tree: $term" )
                                throw e;
                        }
                        val r = exprToTree(rCpsExpr, term)
                        if cpsCtx.flags.debugLevel >= 10 then
                           cpsCtx.log(s"runRoot: rCpsExpr=$rCpsExpr, async=${rCpsExpr.isAsync}")
                           if cpsCtx.flags.debugLevel >= 15 then
                             cpsCtx.log(s"runRoot: r=$r, async=${r.isAsync}, origin=$term")
                        r
                      case _ =>
                        throw MacroError("Can't determinate exact type for term", expr)
     }
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRoot result: $r")
     r


  def runRootUneta(term: qctx.reflect.Term, marker: TransformationContextMarker, muted: Boolean): CpsTree = {
     // TODO: change cpsCtx to show nesting
     if (cpsCtx.flags.debugLevel >= 15 && !muted)
        cpsCtx.log(s"runRootUneta, term=$term")
     val monad = cpsCtx.monad
     val r = term match {
       case Select(qual, name) =>
           runRoot(qual, TransformationContextMarker.Select, muted) match 
              case rq: AsyncCpsTree =>
                  val cTransformed = rq.transformed.asInstanceOf[qctx.reflect.Term]
                  CpsTree.impure(Select(cTransformed,term.symbol),term.tpe)
              case _: PureCpsTree =>
                  CpsTree.pure(term)
       case Ident(name) =>
             CpsTree.pure(term)
       case Apply(x, args) =>
             val thisScope = this
             val nestContext = cpsCtx.nestSame(marker, muted)
             val nestScope = new TreeTransformScope[F,CT] {
                override val cpsCtx = nestContext
                override implicit val qctx = thisScope.qctx
                override val fType = thisScope.fType
                override val ctType = thisScope.ctType
             }
             nestScope.runApply(term.asInstanceOf[nestScope.qctx.reflect.Term],
                                x.asInstanceOf[nestScope.qctx.reflect.Term],
                                args.asInstanceOf[List[nestScope.qctx.reflect.Term]],
                                Nil).asInstanceOf[CpsTree]
       case _ =>
             throw MacroError(s"cps tree transform is not supported yet to ${term}",cpsCtx.patternCode)
     }
     if (cpsCtx.flags.debugLevel >= 15 && !muted)
        cpsCtx.log(s"runRootUneta result: $r  (term=$term)")
     r
  }

  def exprToTree(expr: CpsExpr[F,_], e: Term): CpsTree =
     if (expr.isAsync)
         val transformed = expr.transformed.asTerm
         AwaitSyncCpsTree(transformed, e.tpe)
     else
         PureCpsTree(e)

  object B2{

   def inNestedContext(term: Term, 
                      marker: TransformationContextMarker,
                      muted: Boolean,
                      op: TreeTransformScope[F,?] => CpsTree): CpsTree =
        val nScope = if (false && term.isExpr) {
           term.asExpr match
             case '{ $e: et} =>
                nestScope(e, marker, muted)
             case _ =>
                throw MacroError("Can't determinate type for ${e.show}",posExprs(term))
        } else {
           nestScope(cpsCtx.patternCode, marker, muted)
        }
        op(nScope)


   def nestScope[E:quoted.Type](e: Expr[E], marker: TransformationContextMarker, muted: Boolean): TreeTransformScope[F,E] =
       val et = summon[quoted.Type[E]]
       val nContext = cpsCtx.nest(e, et, marker, muted)
       new TreeTransformScope[F,E] {
            override val cpsCtx = nContext
            override implicit val qctx = thisTransform.qctx
            override val fType = thisTransform.fType
            override val ctType = et
       }

  }



