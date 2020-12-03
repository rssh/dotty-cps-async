package cps.utestlike

// parts copied from utest:

import scala.quoted._

object Dummy

case class TestValue(name: String, tpeName: String, value: Any)

case class AssertEntry[T](label: String, thunk: (TestValue => Unit) => T)

object Tracer {

  def apply[T](func: Expr[Seq[AssertEntry[T]] => Unit], exprs: Expr[Seq[T]])(using Quotes, Type[T]): Expr[Unit] = {
    import quotes.reflect._
    exprs match {
      case Varargs(ess) =>
        val trees: Expr[Seq[AssertEntry[T]]] = Expr.ofSeq(ess.map(e => makeAssertEntry(e, codeOf(e))))
        Expr.betaReduce('{ $func($trees)})

      case _ => throw new RuntimeException(s"Only varargs are supported. Got: ${Term.of(exprs)}")
    }
  }

  private def makeAssertEntry[T](expr: Expr[T], code: String)(using Quotes, Type[T]) =
    import quotes.reflect._
    def entryBody(logger: Expr[TestValue => Unit]) =
      tracingMap(logger).transformTerm(Term.of(expr))(Symbol.spliceOwner).asExprOf[T]
    '{AssertEntry(
      ${Expr(code)},
      logger => ${entryBody('logger)})}

  private def tracingMap(logger: Expr[TestValue => Unit])(using Quotes) =
    import quotes.reflect._
    new TreeMap {
      // Do not descend into definitions inside blocks since their arguments are unbound
      override def transformStatement(tree: Statement)(owner: Symbol): Statement = tree match
        case _: DefDef => tree
        case _ => super.transformStatement(tree)(owner)

      override def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match {
          case i @ Ident(name) if i.symbol.pos.exists
            && i.pos.exists
            // only trace identifiers coming from the same file,
            // since those are the ones people probably care about
            && i.symbol.pos.sourceFile == i.pos.sourceFile
            // Don't trace methods, since you cannot just print them "standalone"
            // without providing arguments
            && !i.symbol.isDefDef && !i.symbol.isClassConstructor
            // Don't trace identifiers which are synthesized by the compiler
            // as part of the language implementation
            && !i.symbol.flags.is(Flags.Artifact)
            // Don't trace "magic" identifiers with '$'s in them
            && !name.toString.contains('$') =>

            tree.tpe.widen.asType match
              case '[t] => wrapWithLoggedValue[t](tree.asExpr, logger)
       // Don't worry about multiple chained annotations for now...
          //case Typed(_, tpt) =>
          //  tpt.tpe match {
          //    case AnnotatedType(underlying, annot) if annot.tpe =:= TypeRepr.of[utest.asserts.Show] =>
          //      underlying.widen.asType match
          //        case '[t] => wrapWithLoggedValue[t](tree.asExpr, logger)
          //    case _ => super.transformTerm(tree)(owner)
          //  }

          // Don't recurse and trace the LHS of assignments
          case t@Assign(lhs, rhs) => Assign.copy(t)(lhs, super.transformTerm(rhs)(owner))

          case _ => super.transformTerm(tree)(owner)
        }
      }
    }

  private def wrapWithLoggedValue[T: Type](expr: Expr[Any], logger: Expr[TestValue => Unit])(using Quotes) = {
    import quotes.reflect._
    val tpeString =
      try Type.show[T]
      catch
        case _ => Type.of[T].toString // Workaround lampepfl/dotty#8858
    expr match {
      case '{ $x: t } =>
        Term.of('{
          val tmp: t = $x
          $logger(TestValue(
            ${Expr(expr.show)},
            //${Expr(StringUtilHelpers.stripScalaCorePrefixes(tpeString))},
            ${Expr(tpeString)},
            tmp
          ))
          tmp
        })
    }
  }

  def codeOf[T](expr: Expr[T])(using Quotes): String =
    import quotes.reflect._
    Term.of(expr).pos.sourceCode



}


object UtestLikeMacro {

  inline def assert(inline exprs: Boolean*): Unit = ${assertProxy('exprs)}

  def assertProxy(exprs: Expr[Seq[Boolean]])(using ctx: Quotes): Expr[Unit] =
    Tracer[Boolean]('{ (esx: Seq[AssertEntry[Boolean]]) => UtestLikeMacro.assertImpl(esx: _*) }, exprs)

  def assertImpl(funcs0: AssertEntry[Boolean]*) = {
    //val funcs = funcs0.toArray
    //val failures = mutable.Buffer.empty[Throwable]
    //var i = 0
    // Avoid using `.map` here, because it makes the stack trace tall, which
    // gets annoying to a user who is trying to look at the call stack and
    // figure out what's going on
    //while(i < funcs.length){
    //  val (res, logged, src) = Util.runAssertionEntry(funcs(i))
    //
    //  def prefix = if (funcs.length == 1) "" else s"#${i+1}: "
    //  res match{
    //    case Success(value) =>
    //      if (!value) throw Util.makeAssertError(prefix + src, logged, null)
    //    case Failure(e) => throw Util.makeAssertError(prefix + src, logged, e)
    //  }
    //
    //  i += 1
    //}

  }


}
