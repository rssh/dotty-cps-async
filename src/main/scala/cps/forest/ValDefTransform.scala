package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class ValDefTransform[F[_]:Type, T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]])
                                     (given qctx: QuoteContext)


     //    case '{ val $x:$tx = $y } => 
  def run[TX:Type](x:Sym[TX],tx:Type[TX],y:Expr[TX]) =
      val tType = summon[Type[T]]
      import qctx.tasty.{_, given}
      import util._
      val tastyTType = tType.unseal.tpe
      println("!!! val detected")
      println(s"f=${f.unseal}")
      val ry = Async.rootTransform[F,TX](y,dm)
      println(s"ry=$ry")
      println(s"x=$x, tx=$tx, y=$y")
      if (ry.haveAwait) 
         val py = ry.transformed
         val fTransformed = '{ ${dm}.map($py)((a:$tx) => {val $x:$tx = a}) }
         val cpsBuild = new CpsChunkBuilder[F,Unit] {
            override def create() = 
                fromFExpr('{ ${dm}.map($py)((a:$tx) => {val $x:$tx = a}) })
            override def append[A:quoted.Type](e: CpsChunk[F,A]) =
                // TODO: inject vlaDef into e
                CpsChunk[F,A](Seq(),
                 '{ 
                    ${dm}.flatMap($py)((a:$tx) => 
                      {
                        val $x: $tx = a
                        ${e.toExpr}
                       })
                    })
                 }
         CpsExprResult[F,T](f, cpsBuild.asInstanceOf[CpsChunkBuilder[F,T]], tType, true)
      else
         val cpsBuild = CpsChunkBuilder.sync(f,dm) 
         CpsExprResult[F,T](f,cpsBuild,tType,false)
     

