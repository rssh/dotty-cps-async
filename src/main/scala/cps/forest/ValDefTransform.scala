package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


object ValDefTransform

     //    case '{ val $x:$tx = $y } => 
  def run[F[_]:Type,T:Type,TX:Type](transformationContext: TransformationContext[F,T],
                  x:Sym[TX],tx:Type[TX],y:Expr[TX])(given qctx: QuoteContext) =
      import transformationContext._
      import qctx.tasty.{_, given}
      import util._
      println(s"!!!ValDefTransform.run:: val detected: x=$x, y=${y.show}")
      val ry = Async.rootTransform[F,TX](y,asyncMonad,false)
      println(s"!!!ValDefTransform.run:: ry=${ry}")
      val unitPatternCode = patternCode.asInstanceOf[Expr[Unit]]
      if (ry.haveAwait) 
         
         val py = ry.transformed
         val cpsBuild = new CpsChunkBuilder[F,Unit] {

            def buildValDef[T](a:Expr[T]):Expr[Unit] =
                 val oldValDef = extractValDef(unitPatternCode)
                 println("oldValDef.symbol=${oldValDef.symbol}")
                 valDefBlock(newValDef(oldValDef,x.name, a.unseal))

            def oldSymbol() = extractValDef(unitPatternCode).symbol

            def fixNewIdent[A](ident:Ident, expr:Expr[A]):Term =
                                substituteIdent(expr.unseal, oldSymbol(), ident)

            def buildAppendBlock(oldValDef: ValDef, rhs:Term, exprTerm:Term):Term = {
                val valDef = ValDef(oldValDef.symbol, Some(rhs))
                exprTerm match {
                  case Block(stats,last) =>
                         Block(valDef::stats, last)
                  case other =>
                         Block(valDef::Nil,other)
                }
            }

            def buildAppendBlockExpr[A](oldValDef: ValDef, rhs:Term, expr:Expr[A]):Expr[A] = 
                 buildAppendBlock(oldValDef,rhs,expr.unseal).seal.asInstanceOf[Expr[A]]

            override def create() = 
                fromFExpr('{ ${asyncMonad}.map($py)((a:$tx) => ${buildValDef('a)}) })

            override def append[A:quoted.Type](e: CpsChunk[F,A]) = {
                val oldValDef = extractValDef(patternCode.asInstanceOf[Expr[Unit]])
                CpsChunk[F,A](Seq(),
                 '{
                    ${asyncMonad}.flatMap($py)((a:$tx) => 
                      ${buildAppendBlockExpr(oldValDef,'a.unseal ,e.toExpr)}
                     )
                 })
            }

            def appendOld[A:quoted.Type](e: CpsChunk[F,A]) =
                // TODO: inject vlaDef into e
                CpsChunk[F,A](Seq(),
                 '{ 
                    ${asyncMonad}.flatMap($py)((a:$tx) => 
                      {
                        ${(let(('a).unseal)(
                           ident => fixNewIdent(ident,e.toExpr)
                         )).seal.asInstanceOf[Expr[F[A]]]}
                       })
                  })

         } // end cpsChunk


         CpsExprResult[F,T](patternCode, cpsBuild.asInstanceOf[CpsChunkBuilder[F,T]], patternType, true)
      else
         // Note, that we can't use CpsChunkBuilder.sync, because we need to
         //  extract origin ValDef from his Block, to allow usage of one from
         //  the enclosing block.
         val cpsBuild = new CpsChunkBuilder[F,T] {
                 override def create() = fromFExpr(
                                 '{ ${asyncMonad}.pure(${patternCode}) })
                 override def append[A:quoted.Type](e:CpsChunk[F,A]) = 
                     val valDef = extractValDef(unitPatternCode)
                     val eExpr = e.toExpr
                     val tree = eExpr.unseal match 
                        case Block(stats, expr) =>
                               Block(valDef::stats, expr)
                        case _ =>
                               Block(valDef::Nil, eExpr.unseal) 
                     CpsChunk[F,A](Seq(),tree.seal.asInstanceOf[Expr[F[A]]])
         } 
         CpsExprResult[F,T](patternCode,cpsBuild,patternType,false)
     
  def newValDef(given qctx: QuoteContext)(oldValDef: qctx.tasty.ValDef, name: String, newRhs: qctx.tasty.Term): qctx.tasty.ValDef = {
         import qctx.tasty.{_,given}
         ValDef.copy(oldValDef)(name,oldValDef.tpt,Some(newRhs))
  }

  def valDefBlock(given qctx:QuoteContext)(v:qctx.tasty.ValDef):Expr[Unit] = {
    import qctx.tasty.{_,given}
    Block(List(v),Literal(Constant(()))).seal.asInstanceOf[Expr[Unit]]
  }

  def extractValDef(given qctx:QuoteContext)(blockExpr:Expr[Unit]): qctx.tasty.ValDef = {
    import qctx.tasty.{_,given}
    blockExpr.unseal match {
      case Block(stats,last) =>
        stats.head match {
          case v: ValDef => v
          case _ => qctx.error("Block with ValDef as first statement expected",blockExpr)
                  ???
        }
      case Inlined(call,binding,body) => extractValDef(body.seal.asInstanceOf[Expr[Unit]])
      case _ => qctx.error("Block expected",blockExpr)
                ??? 
    }
  }
 
  // substitute identifier with the same part.


  def substituteIdent(given qctx:QuoteContext)(tree: qctx.tasty.Term, 
                           origin: qctx.tasty.Symbol, 
                           newIdent: qctx.tasty.Ident): qctx.tasty.Term =
     import qctx.tasty.{_,given}
     import qctx.tasty._
     import util._
     val changes = new TreeMap() {
        override def transformTerm(tree:Term)(given ctx: Context):Term =
          tree match 
            case ident@Ident(name) => if (ident.symbol == origin) {
                                         newIdent
                                      } else {
                                         super.transformTerm(tree)
                                      }
            case _ => super.transformTerm(tree)
     }
     changes.transformTerm(tree)


