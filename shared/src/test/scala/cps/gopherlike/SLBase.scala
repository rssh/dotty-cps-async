package cps.gopherlike

import cps._
import cps.macros._
import cps.macros.forest.TransformUtil

import scala.quoted._
import scala.compiletime._


class SLSelectLoop[F[_]:CpsMonad]:

  case class ReadRecord[A](reader: IFReader[F,A], handler: A=>F[Boolean])
  case class WriteRecord[A](writer: IFWriter[F,A], value: ()=>F[A], handler: A=>F[Boolean])

  var readers: List[ReadRecord[?]] = List.empty
  var writers: List[WriteRecord[?]] = List.empty

  def onRead[A](ch: IFReader[F,A])(f: A=>Boolean): this.type =
       readers = ReadRecord(ch, x => summon[CpsMonad[F]].pure(f(x)))::readers
       this

  def onReadAsync[A](ch:IFReader[F,A])(f: A=>F[Boolean]): this.type =
      readers = ReadRecord(ch,f)::readers
      this

  def onRead_async[A](ch: IFReader[F,A])(f: A=> F[Boolean]): F[this.type] =
      summon[CpsMonad[F]].pure(onReadAsync(ch)(f))

  def onWrite[A](ch: IFWriter[F,A], a: =>A)(f: A=>Boolean): this.type =
      writers = WriteRecord(ch, () => summon[CpsMonad[F]].pure(a), (a) => summon[CpsMonad[F]].pure(f(a)))::writers
      this

  def onWriteAsync[A](ch: IFWriter[F,A], a: ()=>F[A])(f: A=>F[Boolean]): this.type =
      writers = WriteRecord(ch, a, f)::writers
      this

  def onWrite_async[A](ch: IFWriter[F,A], a: ()=>F[A])(f: A=>F[Boolean]): F[this.type] =
      writers = WriteRecord(ch, a, f)::writers
      summon[CpsMonad[F]].pure(this)


  transparent inline def apply(inline pf: PartialFunction[Any,Boolean]): Unit =
    ${  
      SLSelectLoop.loopImpl[F]('pf, '{summon[CpsMonad[F]]})  
    }    
  
  transparent inline def applyAsync(inline pf: PartialFunction[Any,F[Boolean]]): Unit =
    ${
      SLSelectLoop.loopImplAsync[F]('pf, '{summon[CpsMonad[F]]})  
    }
  

  def runAsync():F[Unit] =
    if (readers.isEmpty)
      summon[CpsMonad[F]].pure(())
    else
      summon[CpsMonad[F]].map(readers.head.reader.aread())(a => ())

  transparent inline def run(): Unit =
    await(runAsync())
      
  def fold[S](s0:S)(step: (S,SLSelectLoop[F])=> S|SLSelectLoop.Done[S]): S = {
     fold1[S](s0)(step, 10)
  }

  def fold1[S](s0:S)(step: (S,SLSelectLoop[F])=> S|SLSelectLoop.Done[S], limit: Int): S = {
     if (limit > 0) then
       step(s0, new SLSelectLoop[F] ) match {
         case SLSelectLoop.Done(s) => 
                                     s.asInstanceOf[S]
         case other => fold1(other.asInstanceOf[S])(step, limit-1)
       }
     else
       s0
  }

  def fold_async[S](s0:S)(step: (S,SLSelectLoop[F])=> F[S|SLSelectLoop.Done[S]]): F[S] = {
      fold1_async[S](s0)(step,10)
  }

  def fold1_async[S](s0:S)(step: (S,SLSelectLoop[F])=> F[S|SLSelectLoop.Done[S]], limit: Int): F[S] = {
     if (limit > 0) then
       summon[CpsMonad[F]].flatMap(step(s0, new SLSelectLoop[F])){ r =>
          r match
            case SLSelectLoop.Done(s) => 
                            summon[CpsMonad[F]].pure(s.asInstanceOf[S])
            case other => fold1_async(other.asInstanceOf[S])(step, limit-1)
       }
     else
       summon[CpsMonad[F]].pure(s0)
  }

  transparent inline def afold[S](s0:S)(inline step: (S,SLSelectLoop[F]) => S|SLSelectLoop.Done[S]): F[S] =
     async[F] {
       fold(s0)(step)
     }


  inline def map1[A](inline step: PartialFunction[SLSelectLoop[F], A]): IFReader[F,A] =
     ???
     

object SLSelectLoop:

  case class Done[S](s:S)

  sealed trait SelectGroupExpr[F[_]]:
    def  toExpr: Expr[SLSelectLoop[F]]

  sealed trait SelectorCaseExpr[F[_]:Type]:
     type Monad[X] = F[X]
     def appended(base: Expr[SLSelectLoop[F]])(using Quotes): Expr[SLSelectLoop[F]]
     def appendedAsync(base: Expr[SLSelectLoop[F]])(using Quotes): Expr[SLSelectLoop[F]]

  case class ReadExpression[F[_]:Type, A:Type, B:Type](ch: Expr[IFReader[F,A]], f: Expr[A => B]) extends SelectorCaseExpr[F]:
     def appended(base: Expr[SLSelectLoop[F]])(using Quotes): Expr[SLSelectLoop[F]] =
       '{  
           $base.onRead($ch)(${f.asExprOf[A=>Boolean]}) 
        }

     def appendedAsync(base: Expr[SLSelectLoop[F]])(using Quotes): Expr[SLSelectLoop[F]] =
       '{  
           $base.onReadAsync($ch)(${f.asExprOf[A=>F[Boolean]]}) 
        }
       
  case class WriteExpression[F[_]:Type, A:Type, B:Type](ch: Expr[IFWriter[F,A]], a: Expr[A], f: Expr[A => B], m: Expr[CpsMonad[F]]) extends SelectorCaseExpr[F]:
      def appended(base: Expr[SLSelectLoop[F]])(using Quotes): Expr[SLSelectLoop[F]] =
      '{  $base.onWrite($ch,$a)(${f.asExprOf[A=>Boolean]}) }
   
      def appendedAsync(base: Expr[SLSelectLoop[F]])(using Quotes): Expr[SLSelectLoop[F]] =
      '{  $base.onWriteAsync($ch,()=>$m.pure($a))(${f.asExprOf[A=>F[Boolean]]}) }


  def loopImpl[F[_]:Type](pf: Expr[PartialFunction[Any,Boolean]], m: Expr[CpsMonad[F]])(using Quotes): Expr[Unit] =
      def builder(caseDefs: List[SelectorCaseExpr[F]]):Expr[Unit] = {
          val s0 = '{
              new SLSelectLoop[F](using $m)
          }
          val g: Expr[SLSelectLoop[F]] = caseDefs.foldLeft(s0){(s,e) =>
              e.appended(s)
          }
          val r = '{ $g.run() }
          r.asExprOf[Unit]
      }
      runImpl( builder, pf, m)
      

  def loopImplAsync[F[_]:Type](pf: Expr[PartialFunction[Any,F[Boolean]]], m: Expr[CpsMonad[F]])(using Quotes): Expr[Unit] =
      def builder(caseDefs: List[SelectorCaseExpr[F]]):Expr[Unit] = {
          val s0 = '{
              new SLSelectLoop[F](using $m)
          }
          val g: Expr[SLSelectLoop[F]] = caseDefs.foldLeft(s0){(s,e) =>
              e.appendedAsync(s)
          }
          val r = '{ $g.run() }
          r.asExprOf[Unit]
      }
      runImpl( builder, pf, m)

  def runImpl[F[_]:Type, A:Type,B :Type](builder: List[SelectorCaseExpr[F]]=>Expr[B],
                                 pf: Expr[PartialFunction[Any,A]],
                                 m: Expr[CpsMonad[F]])(using Quotes): Expr[B] =
    import quotes.reflect._
    runImplTree[F,A,B](builder, pf.asTerm, m)

  def runImplTree[F[_]:Type, A:Type, B:Type](using Quotes)(
                builder: List[SelectorCaseExpr[F]] => Expr[B],
                pf: quotes.reflect.Term,
                m: Expr[CpsMonad[F]]
                ): Expr[B] =
    import quotes.reflect._
    pf match
      case Lambda(valDefs, body) =>
        runImplTree[F,A,B](builder, body, m)
      case Inlined(_,List(),body) => 
        runImplTree[F,A,B](builder, body, m)
      case Match(scrutinee,cases) =>
        builder(cases.map(parseCaseDef[F,A](_,m)))
      case _ =>
        println(s"can't parse at runImpl, code=${pf.show}")
        reportError("unsupported case", pf.asExpr)
    

  def parseCaseDef[F[_]:Type,S:Type](using qctx: Quotes)(caseDef: quotes.reflect.CaseDef, m:Expr[CpsMonad[F]]): SelectorCaseExpr[F] =
    import qctx.reflect._
    caseDef.pattern match 
      case Inlined(_,List(),body) => 
            parseCaseDef(CaseDef(body, caseDef.guard, caseDef.rhs),m)
      case b@Bind(v, tp@Typed(expr, TypeSelect(ch,"read"))) =>
          val readFun = makeLambda(v,tp.tpe,b.symbol,caseDef.rhs)
          if (ch.tpe <:< TypeRepr.of[IFReader[F,?]]) 
            tp.tpe.widen.asType match
              case '[a] => 
                TransformUtil.veryWiden(caseDef.rhs.tpe).asType match
                  case '[r] =>
                     ReadExpression(ch.asExprOf[IFReader[F,a]],readFun.asExprOf[a=>r])
                  case _ => 
                     reportError("can't determinate result type", readFun.asExpr)
              case _ => 
                reportError("can't determinate read type", caseDef.pattern.asExpr)
          else
            reportError("read pattern is not a read channel", ch.asExpr)
      case b@Bind(v, tp@Typed(expr, TypeSelect(ch,"write"))) =>
          val writeFun = makeLambda(v,tp.tpe, b.symbol, caseDef.rhs)
          val e = matchCaseDefCondition(caseDef, v)
          if (ch.tpe <:< TypeRepr.of[IFWriter[F,?]]) then
            tp.tpe.widen.asType match
              case '[a] => 
                TransformUtil.veryWiden(caseDef.rhs.tpe).asType match
                  case '[r] =>
                     WriteExpression(ch.asExprOf[IFWriter[F,a]],e.asExprOf[a], writeFun.asExprOf[a=>r], m) 
                  case _ => 
                     reportError("can't determinate result type", writeFun.asExpr)
              case _ =>
                reportError("Can't determinate type of write", tp.asExpr) 
          else
            reportError("Write channel expected", ch.asExpr)
      //case b@Bind(v, tp@Typed(expr, TypeSelect(ch,"after"))) =>
      //    val timeoutFun = makeLambda(v, tp.tpe, b.symbol, caseDef.rhs)
      //    val e = matchCaseDefCondition(caseDef, v)
      //    if (ch.tpe <:< TypeRepr.of[gopher.Time] ||  ch.tpe <:< TypeRepr.of[gopher.Time.type]) 
      //       TimeoutExpression(e.asExprOf[FiniteDuration], timeoutFun.asExprOf[FiniteDuration => S])
      //    else
      //      reportError(s"Expected Time, we have ${ch.show}", ch.asExpr) 
      //case b@Bind(v, tp@Typed(expr, TypeSelect(ch,"done"))) =>
      //    val readFun = makeLambda(v,tp.tpe,b.symbol,caseDef.rhs)
      //    tp.tpe.asType match
      //      case '[a] => 
      //          if (ch.tpe <:< TypeRepr.of[ReadChannel[F,a]]) then
      //              DoneExression(ch.asExprOf[ReadChannel[F,a]],readFun.asExprOf[Unit=>S])
      //          else
      //            reportError("done base is not a read channel", ch.asExpr) 
      //      case _ =>
      //          reportError("can't determinate read type", caseDef.pattern.asExpr)
      case _ =>
        report.error(
          s"""
              expected one of: 
                     v: channel.read
                     v: channel.write if v == expr
                     v: Time.after if v == expr
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
    val mt = MethodType(List(argName))(_ => List(argType), _ => TransformUtil.veryWiden(body.tpe))
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




