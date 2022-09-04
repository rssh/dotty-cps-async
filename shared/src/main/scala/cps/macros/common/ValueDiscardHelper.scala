package cps.macros.common

import scala.quoted.*

import cps.*
import cps.macros.*
import cps.macros.misc.*


object ValueDiscardHelper {


  def checkValueDiscarded(using Quotes)(t: quotes.reflect.Term, flags: AsyncMacroFlags): Boolean =
    import quotes.reflect.*
    ( (flags.customValueDiscard || flags.warnValueDiscard)
     &&
      ( !(t.tpe =:= TypeRepr.of[Unit]) && !(t.tpe =:= TypeRepr.of[Nothing]) )
    )

  def searchCustomDiscardFor(using Quotes)(t: quotes.reflect.Term): quotes.reflect.ImplicitSearchResult =
    import quotes.reflect.*
    val valueDiscard = TypeIdent(Symbol.classSymbol("cps.ValueDiscard")).tpe
    val tpe = t.tpe.widen.dealias
    val tpTree = valueDiscard.appliedTo(tpe)
    Implicits.search(tpTree)
  

  def buildAwaitValueDiscard[F[_]:Type](using Quotes)(discardTerm: quotes.reflect.Term, p: quotes.reflect.Term, 
                                    monad:Expr[CpsMonad[F]], monadContext: Expr[CpsMonadContext[F]],
                                    buildAwait: (quotes.reflect.TypeRepr, quotes.reflect.TypeRepr, quotes.reflect.Term) => quotes.reflect.Term  ): quotes.reflect.Term =
    import quotes.reflect.*
  
    def parseDiscardTermType(tpe: TypeRepr): (TypeRepr, TypeRepr) =
        tpe match
           case AppliedType(base, targs) =>
                  base match
                    case TypeRef(sup, "AwaitValueDiscard") =>
                       targs match
                         case List(tf, tt) => (tf, tt)
                         case _ =>
                             val msg = s"Expected that AwaitValueDiscard have 2 type paraleters, but we have $targs"
                             throw MacroError(msg, discardTerm.asExpr)
                    case _ =>
                       val msg = s"Reference to AwaitValueDiscard expected"
                       throw MacroError(msg, discardTerm.asExpr)
           case _ =>
                  val msg = s"Can't parse AwaitValueDiscard type, tpe=${tpe}"
                  throw MacroError(msg, discardTerm.asExpr)

    discardTerm.tpe.asType match
        case '[AwaitValueDiscard[F,tt]] =>
           buildAwait(TypeRepr.of[F], TypeRepr.of[tt], p)
           //'{  await[F,tt,F]($refP)(using ${monad}, ${monadContext})  }.asTerm
           //bug in dotty. TODO: submit
           //case '[AwaitValueDiscard[[xt]=>>ft,tt]] =>
           //   ???
        case _ => 
           val (ftr, ttr) = parseDiscardTermType(discardTerm.tpe)
           buildAwait(ftr,ttr,p)
           //val ftmt = TypeRepr.of[CpsMonad].appliedTo(ftr)
           //Implicits.search(ftmt) match
           //   case monadSuccess: ImplicitSearchSuccess =>
           //     val ftm = monadSuccess.tree
           //     buildAwait(ftr,ttr,p)
           //     Apply(    
           //          Apply(
           //            TypeApply(Ref(Symbol.requiredMethod("cps.await")), 
           //               List(Inferred(ftr),Inferred(ttr),Inferred(ftr))),
           //            List(p)
           //          ),
           //          List(ftm, monadContext.asTerm)
           //     )
           //   case monadFailure: ImplicitSearchFailure =>
           //     throw MacroError(s"Can't find appropriative monad for ${discardTerm.show}, ${monadFailure.explanation}  : ", p.asExpr)



}

