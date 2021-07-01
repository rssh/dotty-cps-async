package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


trait KnownTreeFragments[F[_], CT]:

  thisKnownTreeTransform: TreeTransformScope[F, CT] =>

  import qctx.reflect._

  lazy val awaitPure = '{ _root_.cps.await[F,Int](${cpsCtx.monad}.pure(3))(using ${cpsCtx.monad}) }.asTerm

  lazy val awaitSymbol = Symbol.requiredMethod("cps.await")

  lazy val monadTypeTree = TransformUtil.find(awaitPure,
                       { case TypeApply(Select(x,"await"),List(f1,f2)) => Some(f1)
                         case _ => None
                       }).get.asInstanceOf[TypeTree]


  lazy val pureSymbol = TransformUtil.find(awaitPure,
                           { case v@Select(x,m) if m == "pure" => Some(v)
                             case _ => None
                           }).get.symbol


  lazy val mapSymbol = {
        val mapTmpl = '{ ${cpsCtx.monad}.map(${cpsCtx.monad}.pure(3))(_ + 1)  }.asTerm

        TransformUtil.find(mapTmpl,
                           { case v@Select(x,m) if m == "map" => Some(v)
                             case _ => None
                           }).get.symbol
  }


  lazy val flatMapSymbol = {
        val flatMapTmpl = '{ ${cpsCtx.monad}.flatMap(${cpsCtx.monad}.pure(3))( x =>
                                                                ${cpsCtx.monad}.pure(1 + x))  }.asTerm

        TransformUtil.find(flatMapTmpl,
                           { case v@Select(x,m) if m == "flatMap" => Some(v)
                             case _ => None
                           }).get.symbol
  }


  lazy val objAsyncShift = TypeIdent(Symbol.classSymbol("cps.ObjectAsyncShift")).tpe

  lazy val partialFunctionType = TypeIdent(Symbol.classSymbol("scala.PartialFunction")).tpe

