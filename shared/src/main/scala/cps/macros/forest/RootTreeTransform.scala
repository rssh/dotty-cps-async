// root transform for low-level tasty trees.
//  (C) Ruslan Shevchenko, 2019-2022, Kiev, Ukraine
package cps.macros.forest

import scala.quoted._
import scala.util.control.NonFatal

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._


trait RootTreeTransform[F[_], CT, CC <: CpsMonadContext[F] ]:

  thisTransform: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  def runRoot(term: qctx.reflect.Term, muted: Boolean = false)(owner: Symbol): CpsTree =
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRoot: (nesting = ${cpsCtx.nesting} term=${safeShow(term)}")
     val r: CpsTree = term.tpe.widen match {
       case _ : MethodType =>
               //  in such case, we can't transform tree to expr
               //  without eta-expansion.
               //    from other side - we don't want do eta-expand now, it can be performed early.
                runRootUneta(term, muted)(owner)
       case _ : PolyType =>
                runRootUneta(term, muted)(owner)
       case _ =>
                term match
                  case lambdaTerm@Lambda(params, body) =>
                                 // type of cps[x => y]  is  x=>F[y], not F[X=>Y]
                                 //  and if it violate CpsExpr contract (which require F[X=>Y]), let's
                                 //  work with lambda on the tree level.
                            B2.inNestedContext(lambdaTerm, owner, muted, scope =>
                                 scope.runLambda(lambdaTerm.asInstanceOf[scope.qctx.reflect.Term],
                                                 params.asInstanceOf[List[scope.qctx.reflect.ValDef]],
                                                 body.asInstanceOf[scope.qctx.reflect.Term])
                                                 (owner.asInstanceOf[scope.qctx.reflect.Symbol]).inCake(thisTransform)
                            )
                  case applyTerm@Apply(fun,args)  =>
                            if (cpsCtx.flags.debugLevel >= 15)
                               cpsCtx.log(s"runRoot: applyTerm=$applyTerm, fun=${fun.show}")
                            val tree = B2.inNestedContext(applyTerm, owner, muted, scope =>
                               scope.runApply(applyTerm.asInstanceOf[scope.qctx.reflect.Apply],
                                              fun.asInstanceOf[scope.qctx.reflect.Term],
                                              args.asInstanceOf[List[scope.qctx.reflect.Term]],
                                              Nil)(owner.asInstanceOf[scope.qctx.reflect.Symbol]).inCake(thisTransform)
                            )
                            tree.inCake(thisTransform)
                  case inlined@Inlined(call,bindings,body) =>
                            val tree = B2.inNestedContext(inlined, owner,  muted, scope =>
                               scope.runInlined(inlined.asInstanceOf[scope.qctx.reflect.Inlined])
                                               (owner.asInstanceOf[scope.qctx.reflect.Symbol])
                                    .inCake(thisTransform)
                            )
                            tree          
                  // special case, until we not enabled total blcok          
                  case minBlock@Block(Nil, last) =>
                             val cpsLast = runRoot(last, muted = muted)(owner)
                             cpsLast
                  // should be enabled after owner-in-cps-tree           
                  //case block@Block(stats,last) =>
                  //           val tree = B2.inNestedContext(block, muted, scope =>
                  //             scope.runBlock(
                  //               block.asInstanceOf[scope.qctx.reflect.Block],
                  //               stats.map(x => x.asInstanceOf[scope.qctx.reflect.Statement]),
                  //               last.asInstanceOf[scope.qctx.reflect.Term]
                  //             ).inCake(thisTransform) 
                  //           )
                  //           tree
                  case _ =>  // TODO: elimi
                    val expr = term.asExpr
                    val monad = cpsCtx.monad
                    TransformUtil.veryWiden(term.tpe).asType match
                      case '[ et ] =>
                        val rCpsExpr = try {
                             if cpsCtx.flags.debugLevel >= 15 then
                                cpsCtx.log(s"nextedTransfornm: orin = $term")
                             val nFlags = cpsCtx.flags.copy(muted = muted || cpsCtx.flags.muted)
                             Async.nestTransform(term.asExprOf[et], cpsCtx.copy(flags = nFlags))
                        } catch {
                             case e: MacroError  =>
                                if (!e.printed) then
                                  val termShowed = try {
                                     term.show
                                  } catch {
                                     case NonFatal(ex) => term.toString
                                  }
                                  println(s"can't translate tree: ${termShowed}" )
                                  if (cpsCtx.flags.debugLevel > 0) then
                                     e.printStackTrace()
                                  throw e.copy(printed=true);
                                else
                                  throw e
                        }
                        val r: CpsTree = exprToTree(rCpsExpr, term)(owner)
                        if cpsCtx.flags.debugLevel >= 10 then
                           cpsCtx.log(s"runRoot: rCpsExpr=${rCpsExpr.show}, async=${rCpsExpr.isAsync}")
                           if cpsCtx.flags.debugLevel >= 15 then
                             cpsCtx.log(s"runRoot: r=$r")
                             cpsCtx.log(s"runRoot: origin=$term")
                        r
                      case _ =>
                        throw MacroError("Can't determinate exact type for term", expr)
     }
     if (cpsCtx.flags.debugLevel >= 15)
        cpsCtx.log(s"runRoot result: $r")
     r


  def runRootUneta(term: qctx.reflect.Term, muted: Boolean)(owner: Symbol): CpsTree = {
     // TODO: change cpsCtx to show nesting
     if (cpsCtx.flags.debugLevel >= 15 && !muted)
        cpsCtx.log(s"runRootUneta, term=$term")
     val monad = cpsCtx.monad
     val r = term match {
       case Select(qual, name) =>
             val cpsQual = runRoot(qual, muted)(owner)
             cpsQual.select(term, term.symbol, term.tpe.widen)
       case Ident(name) =>
             CpsTree.pure(owner,term)
       case applyTerm@Apply(x, args) =>
             val thisScope = this
             val nestContext = cpsCtx.nestSame(muted)
             val nestScope = new TreeTransformScope[F,CT,CC] {
                override val cpsCtx = nestContext
                override implicit val qctx = owner.asQuotes
                override val fType = thisScope.fType
                override val ctType = thisScope.ctType
                override val ccType = thisScope.ccType
             }
             nestScope.runApply(applyTerm.asInstanceOf[nestScope.qctx.reflect.Apply],
                                x.asInstanceOf[nestScope.qctx.reflect.Term],
                                args.asInstanceOf[List[nestScope.qctx.reflect.Term]],
                                Nil)(owner.asInstanceOf[nestScope.qctx.reflect.Symbol]).inCake(thisTransform)
       case _ =>
             throw MacroError(s"cps tree transform is not supported yet to ${term}",cpsCtx.patternCode)
     }
     if (cpsCtx.flags.debugLevel >= 15 && !muted)
        cpsCtx.log(s"runRootUneta result: $r  (term=$term)")
     r
  }

  def exprToTree[T](expr: CpsExpr[F,T], e: Term)(owner: Symbol): CpsTree =
     expr.syncOrigin match
       case Some(origin) => 
            PureCpsTree(owner, origin.asTerm, expr.isChanged)
       case None => 
           val transformed = expr.transformed.asTerm
           AwaitSyncCpsTree(owner, transformed.changeOwner(owner), e.tpe.widen)
     
     //if (expr.isAsync)
     //    val transformed = expr.transformed.asTerm
     //    AwaitSyncCpsTree(transformed, e.tpe.widen)
     //else
         //PureCpsTree(e)

  object B2{

   def inNestedContext(term: Term,
                      owner: Symbol,
                      muted: Boolean,
                      op: TreeTransformScope[F,?, ?] => CpsTree): CpsTree =
        val nScope = if (false && term.isExpr) {
           term.asExpr match
             case '{ $e: et} =>
                nestScope(e, owner, muted)
             case _ =>
                throw MacroError("Can't determinate type for ${e.show}",posExprs(term))
        } else {
           nestScope(cpsCtx.patternCode, owner, muted)
        }
        op(nScope)


   def nestScope[E:quoted.Type](e: Expr[E], owner: Symbol, muted: Boolean): TreeTransformScope[F,E,CC] =
       val et = summon[quoted.Type[E]]
       val cct = summon[quoted.Type[CC]]
       val nContext = cpsCtx.nest(e, et, muted)
       new TreeTransformScope[F,E,CC] {
            override val cpsCtx = nContext
            override implicit val qctx = owner.asQuotes
            override val fType = thisTransform.fType
            override val ctType = et
            override val ccType = cct
       }

  }



