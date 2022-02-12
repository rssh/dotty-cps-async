package cps.gopherlike2

import cps._
import cps.macros._

import scala.quoted._
import scala.compiletime._

import cps.macros.forest.TransformUtil

class SLSelect[F[_], S](m:CpsMonad[F]):

  def asyncMonad: CpsMonad[F] = m

  case class ReadRecord[A](reader: IFReader[F,A], handler: A=>F[S])
  case class WriteRecord[A](writer: IFWriter[F,A], value: ()=>F[A], handler: A=>F[S])

  var readers: List[ReadRecord[?]] = List.empty
  var writers: List[WriteRecord[?]] = List.empty

  def onRead[A](ch: IFReader[F,A])(f: A=> S): this.type =
       readers = ReadRecord(ch, x => asyncMonad.pure(f(x)))::readers
       this

  def onReadAsync[A](ch:IFReader[F,A])(f: A=>F[S]): this.type =
      readers = ReadRecord(ch,f)::readers
      this

  def onRead_async[A](ch: IFReader[F,A])(f: A=> F[S]): F[this.type] =
      asyncMonad.pure(onReadAsync(ch)(f))

  def onWrite[A](ch: IFWriter[F,A], a: =>A)(f: A=>S): this.type =
      writers = WriteRecord(ch, () => asyncMonad.pure(a), (a) => asyncMonad.pure(f(a)))::writers
      this

  def onWrite_async[A](ch: IFWriter[F,A], a: ()=>F[A])(f: A=>F[S]): F[this.type] =
      writers = WriteRecord(ch, a, f)::writers
      asyncMonad.pure(this)


  transparent inline def apply(inline pf: PartialFunction[Any,S])(using inline mc:CpsMonadContext[F]): S =
    ${  
      SLSelect.applyImpl[F,S]('pf, '{summonInline[CpsMonad[F]]}, 'mc )  
    }    
  
  transparent inline def apply1[A](inline ch: IFReader[F,A], f: A=>S)(using CpsMonadContext[F]): S =
      val s0 = new SLSelect[F,S](asyncMonad)
      await(s0.onRead(ch)(f).runAsync())(using asyncMonad)
  
  inline def apply2[A](inline pf: PartialFunction[Any,S]): S =
      ???
  

  def runAsync():F[S] = 
      throw new RuntimeException("TestCase:runAsync:NotImplemented")

  transparent inline def run()(using mc: CpsMonadContext[F]): S =
    await(runAsync())(using asyncMonad, mc)
      
  def fold[S](s0:S)(step: (S,SLSelect[F,S])=> S|SLSelect.Done[S]): S = {
     step(s0, new SLSelect[F,S](m) ) match {
         case SLSelect.Done(s) => s.asInstanceOf[S]
         case other => fold(other.asInstanceOf[S])(step)
     }
  }


  def fold_async[S](s0:S)(step: (S,SLSelect[F,S])=> F[S|SLSelect.Done[S]]): F[S] = {
     asyncMonad.flatMap(step(s0, new SLSelect[F,S](asyncMonad))){ r =>
        r match
          case SLSelect.Done(s) => asyncMonad.pure(s.asInstanceOf[S])
          case other => fold_async(other.asInstanceOf[S])(step)
     }
  }


  inline def afold[S](s0:S)(inline step: (S,SLSelect[F,S]) => S|SLSelect.Done[S]): F[S] =
     async[F](using asyncMonad).apply{
       fold(s0)(step)
     }


     

object SLSelect:

  case class Done[S](s:S)

  sealed trait SelectGroupExpr[F[_],S]:
    def  toExpr: Expr[SLSelect[F,S]]

  sealed trait SelectorCaseExpr[F[_]:Type,S:Type]:
     type Monad[X] = F[X]
     def appended(base: Expr[SLSelect[F,S]])(using Quotes): Expr[SLSelect[F,S]]

  case class ReadExpression[F[_]:Type, A:Type, S:Type](ch: Expr[IFReader[F,A]], f: Expr[A => S]) extends SelectorCaseExpr[F,S]:
     def appended(base: Expr[SLSelect[F,S]])(using Quotes): Expr[SLSelect[F,S]] =
       '{  $base.onRead($ch)($f) }
       
  case class WriteExpression[F[_]:Type, A:Type, S:Type](ch: Expr[IFWriter[F,A]], a: Expr[A], f: Expr[A => S]) extends SelectorCaseExpr[F,S]:
      def appended(base: Expr[SLSelect[F,S]])(using Quotes): Expr[SLSelect[F,S]] =
      '{  $base.onWrite($ch,$a)($f) }
   

  def loopImpl[F[_]:Type](pf: Expr[PartialFunction[Any,Boolean]], m: Expr[CpsMonad[F]], mc: Expr[CpsMonadContext[F]])(using Quotes): Expr[Unit] =
      def builder(caseDefs: List[SelectorCaseExpr[F,Boolean]]):Expr[Unit] = {
          val s0 = '{
              new SLSelect[F,Boolean]($m)
          }
          val g: Expr[SLSelect[F,Boolean]] = caseDefs.foldLeft(s0){(s,e) =>
              e.appended(s)
          }
          val r = '{ await($g.runAsync())(using $m, $mc) }
          r.asExprOf[Unit]
      }
      runImpl( builder, pf)
      

  def applyImpl[F[_]:Type, S:Type](pf: Expr[PartialFunction[Any,S]], m: Expr[CpsMonad[F]], mc: Expr[CpsMonadContext[F]])(using Quotes): Expr[S] =
      def builder(caseDefs: List[SelectorCaseExpr[F,S]]):Expr[S] = {
          val s0 = '{
              new SLSelect[F,S]($m)
          }
          val g: Expr[SLSelect[F,S]] = caseDefs.foldLeft(s0){(s,e) =>
              e.appended(s)
          }
          val r = '{ await($g.runAsync())(using $m, $mc) }
          r
      }
      runImpl( builder, pf)

  def runImpl[F[_]:Type, A:Type, B :Type](builder: List[SelectorCaseExpr[F,A]]=>Expr[B],
                                 pf: Expr[PartialFunction[Any,A]])(using Quotes): Expr[B] =
    import quotes.reflect._
    runImplTree[F,A,B](builder, pf.asTerm)

  def runImplTree[F[_]:Type, A:Type, B:Type](using Quotes)(
                builder: List[SelectorCaseExpr[F,A]] => Expr[B],
                pf: quotes.reflect.Term
                ): Expr[B] =
    import quotes.reflect._
    pf match
      case Lambda(valDefs, body) =>
        runImplTree[F,A,B](builder, body)
      case Inlined(_,List(),body) => 
        runImplTree[F,A,B](builder, body)
      case Match(scrutinee,cases) =>
        builder(cases.map(parseCaseDef[F,A](_)))
    

  def parseCaseDef[F[_]:Type,S:Type](using qctx: Quotes)(caseDef: quotes.reflect.CaseDef): SelectorCaseExpr[F,S] =
    import qctx.reflect._
    caseDef.pattern match 
      case Inlined(_,List(),body) => 
            parseCaseDef(CaseDef(body, caseDef.guard, caseDef.rhs))
      case b@Bind(v, tp@Typed(expr, TypeSelect(ch,"read"))) =>
          val readFun = makeLambda(v,tp.tpe,b.symbol,caseDef.rhs)
          if (ch.tpe <:< TypeRepr.of[IFReader[F,?]]) 
            TransformUtil.veryWiden(tp.tpe).asType match
              case '[a] => 
                ReadExpression(ch.asExprOf[IFReader[F,a]],readFun.asExprOf[a=>S])
              case _ => 
                reportError("can't determinate read type", caseDef.pattern.asExpr)
          else
            reportError("read pattern is not a read channel", ch.asExpr)
      case b@Bind(v, tp@Typed(expr, TypeSelect(ch,"write"))) =>
          val writeFun = makeLambda(v,tp.tpe, b.symbol, caseDef.rhs)
          val e = matchCaseDefCondition(caseDef, v)
          if (ch.tpe <:< TypeRepr.of[IFWriter[F,?]]) then
            TransformUtil.veryWiden(tp.tpe).asType match
              case '[a] => 
                WriteExpression(ch.changeOwner(Symbol.spliceOwner).asExprOf[IFWriter[F,a]],e.asExprOf[a], writeFun.asExprOf[a=>S]) 
              case _ =>
                reportError("Can't determinate type of write", tp.asExpr) 
          else
            reportError("Write channel expected", ch.asExpr)
      case _ =>
        report.error(
          s"""
              expected one of: 
                     v: channel.read
                     v: channel.write if v == expr
              we have
                    ${caseDef.pattern.show}
          """, caseDef.pattern.asExpr)
        reportError(s"unparsed caseDef pattern: ${caseDef.pattern}", caseDef.pattern.asExpr)
        
  end parseCaseDef

  def matchCaseDefCondition(using Quotes)(caseDef: quotes.reflect.CaseDef, v: String): quotes.reflect.Term =
    import quotes.reflect._
    caseDef.guard match
      case Some(condition) =>
        condition match
          case Apply(quotes.reflect.Select(Ident(v1),method),List(expr)) =>
            if (v1 != v) {
               reportError(s"write name mismatch ${v1}, expected ${v}", condition.asExpr)
            }
            // TODO: check that method is '==''
            expr
          case _ =>  
            reportError(s"Condition is not in form x==expr,${condition} ",condition.asExpr)
      case _ =>
        reportError(s"Condition is required ",caseDef.pattern.asExpr)
    

  def makeLambda(using Quotes)(argName: String, 
                argType: quotes.reflect.TypeRepr, 
                oldArgSymbol: quotes.reflect.Symbol,
                body: quotes.reflect.Term): quotes.reflect.Block =
    import quotes.reflect._
    val mt = MethodType(List(argName))(_ => List(argType), _ => body.tpe.widen)
    Lambda(Symbol.spliceOwner, mt, (owner,args) =>
      substIdent(body,oldArgSymbol, args.head.asInstanceOf[Term], owner).changeOwner(owner))

    
  def substIdent(using Quotes)(term: quotes.reflect.Term, 
                                 fromSym: quotes.reflect.Symbol, 
                                 toTerm: quotes.reflect.Term,
                                 owner: quotes.reflect.Symbol): quotes.reflect.Term = 
      import quotes.reflect._
      val argTransformer = new TreeMap() {
        override def transformTerm(tree: Term)(owner: Symbol):Term =
          tree match
            case Ident(name) if tree.symbol == fromSym => toTerm
            case _ => super.transformTerm(tree)(owner)
      }
      argTransformer.transformTerm(term)(owner)

  def reportError(message: String, posExpr: Expr[?])(using Quotes): Nothing =
    import quotes.reflect._
    report.error(message, posExpr)
    throw new RuntimeException(s"Error in macro: $message")




