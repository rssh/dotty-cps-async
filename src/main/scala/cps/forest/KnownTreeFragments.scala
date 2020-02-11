package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


trait KnownTreeFragments[F[_]]:

  thisKnownTreeTransform: TreeTransformScope[F] =>
  
  import qctx.tasty.{_, given}

  lazy val awaitPure = TransformUtil.skipInlined(
                         '{ _root_.cps.await[F,Int](${cpsCtx.asyncMonad}.pure(3)) }.unseal)

  lazy val awaitSymbol = TransformUtil.find(awaitPure,
                           { case v@Select(x,m) if m == "await" => Some(v)
                             case v@Ident("await") => Some(v)
                             case _ => None
                           }).get.symbol

  lazy val monadTypeTree = TransformUtil.find(awaitPure,
                       { case TypeApply(Select(x,"await"),List(f1,f2)) => Some(f1)
                         case _ => None
                       }).get.asInstanceOf[TypeTree]


  lazy val mapSymbol = {
        val mapTmpl = TransformUtil.skipInlined(
                          '{ ${cpsCtx.asyncMonad}.map(${cpsCtx.asyncMonad}.pure(3))(_ + 1)  }
                         .unseal)

        TransformUtil.find(mapTmpl,
                           { case v@Select(x,m) if m == "map" => Some(v)
                             case _ => None
                           }).get.symbol
  }


  lazy val flatMapSymbol = {
        val flatMapTmpl = TransformUtil.skipInlined(
                          '{ ${cpsCtx.asyncMonad}.flatMap(${cpsCtx.asyncMonad}.pure(3))( x =>
                                                                ${cpsCtx.asyncMonad}.pure(1 + x))  }
                         .unseal)
     
        TransformUtil.find(flatMapTmpl,
                           { case v@Select(x,m) if m == "flatMap" => Some(v)
                             case _ => None
                           }).get.symbol
  }


