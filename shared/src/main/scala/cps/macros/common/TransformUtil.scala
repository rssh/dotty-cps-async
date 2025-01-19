package cps.macros.common

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._

object TransformUtil:

  def createFunctionType(using
      Quotes
  )(argTypes: List[quotes.reflect.TypeRepr], resultType: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect._
    val funSymbol = defn.FunctionClass(argTypes.size)
    val funTypeTree: TypeTree = TypeIdent(funSymbol)
    funTypeTree.tpe.appliedTo(argTypes.map(_.widen) :+ resultType.widen)

  def substituteLambdaParams(using
      qctx: Quotes
  )(
      oldParams: List[quotes.reflect.ValDef],
      newParams: List[quotes.reflect.Tree],
      body: quotes.reflect.Term,
      owner: quotes.reflect.Symbol
  ): quotes.reflect.Term =
    import quotes.reflect._
    val paramsMap = oldParams.zipWithIndex.map { case (tree, index) => (tree.symbol, index) }.toMap
    val indexedArgs = newParams.toIndexedSeq

    // def lookupParamTerm(sym: Symbol): Option[Term] =
    //      paramsMap.get(sym).map(i => Ref(indexedArgs(i).symbol))
    val assoc: Map[Symbol, Tree] = paramsMap.map((k, i) => (k, Ref(indexedArgs(i).symbol)))
    changeSymsInTerm(assoc, body, owner)

  def changeSymsInTerm(using
      Quotes
  )(
      association: Map[quotes.reflect.Symbol, quotes.reflect.Tree],
      body: quotes.reflect.Term,
      owner: quotes.reflect.Symbol
  ): quotes.reflect.Term = {
    changeSymsInTree(association, body, owner).asInstanceOf[quotes.reflect.Term]
  }

  def changeSymsInTree(using
      qctx: Quotes
  )(
      association: Map[qctx.reflect.Symbol, qctx.reflect.Tree],
      body: quotes.reflect.Tree,
      owner: quotes.reflect.Symbol
  ): quotes.reflect.Tree =
    import quotes.reflect._

    // TODO: mege wirh changeSyms
    val argTransformer = new TreeMap() {

      def lookupParamTerm(sym: Symbol): Option[Term] =
        association.get(sym) match
          case Some(paramTree) =>
            paramTree match
              case paramTerm: Term => Some(paramTerm)
              case _ =>
                throw MacroError(s"term expected for lambda param, we have ${paramTree}", body.asExpr)
          case _ => None

      override def transformTree(tree: Tree)(owner: Symbol): Tree =
        tree match
          case pattern: Bind =>
            Bind.copy(pattern)(pattern.name, transformTree(pattern.pattern)(owner))
          case _ =>
            super.transformTree(tree)(owner)

      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case Ident(name) =>
            lookupParamTerm(tree.symbol) match
              case Some(paramTerm) => paramTerm
              case _               => super.transformTerm(tree)(owner)
          case _ => super.transformTerm(tree)(owner)

      override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
        tree match
          case Singleton(ref) =>
            lookupParamTerm(ref.symbol) match
              case Some(paramTerm) => Singleton(paramTerm)
              case None            => super.transformTypeTree(tree)(owner)
          case a @ Annotated(tp, annotation) =>
            // bug in default TreeTransform, should process Annotated
            Annotated.copy(a)(transformTypeTree(tp)(owner), transformTerm(annotation)(owner))
          case i @ Inferred() =>
            Inferred(transformType(i.tpe)(owner))
          case t: TypeSelect =>
            TypeSelect.copy(t)(transformTerm(t.qualifier)(owner), t.name)
          case _ =>
            super.transformTypeTree(tree)(owner)

      def transformType(tp: TypeRepr)(owner: Symbol): TypeRepr =
        tp match
          case ConstantType(c) => tp
          case tref @ TermRef(qual, name) =>
            lookupParamTerm(tref.termSymbol) match
              case Some(paramTerm) => paramTerm.tpe
              case None            => tp
          case tp @ TypeRef(internal, name) =>
            internal match
              case tr: TermRef =>
                lookupParamTerm(tr.termSymbol) match
                  case Some(paramTerm) =>
                    TypeSelect(paramTerm, name).tpe
                  case None =>
                    tp
              case _ =>
                // we can't get inside, since it is
                tp
          case SuperType(thisTpe, superTpe) =>
            SuperType(transformType(thisTpe)(owner), transformType(superTpe)(owner))
          case Refinement(parent, name, info) =>
            Refinement(transformType(parent)(owner), name, transformType(info)(owner))
          case AppliedType(tycon, args) =>
            transformType(tycon)(owner).appliedTo(args.map(x => transformType(x)(owner)))
          case AnnotatedType(underlying, annot) =>
            AnnotatedType(transformType(underlying)(owner), transformTerm(annot)(owner))
          case AndType(rhs, lhs) => AndType(transformType(rhs)(owner), transformType(lhs)(owner))
          case OrType(rhs, lhs)  => OrType(transformType(rhs)(owner), transformType(lhs)(owner))
          case MatchType(bound, scrutinee, cases) =>
            MatchType(transformType(bound)(owner), transformType(scrutinee)(owner), cases.map(x => transformType(x)(owner)))
          case ByNameType(tp1)     => ByNameType(transformType(tp1)(owner))
          case ParamRef(x, index)  => tp // transform tp ?
          case NoPrefix()          => tp
          case TypeBounds(low, hi) => TypeBounds(transformType(low)(owner), transformType(hi)(owner))
          case _                   => tp

    }
    argTransformer.transformTree(body)(owner)

  /** widen, which works over 'or' and 'and' types. (bug in dotty?)
    */
  def veryWiden(using qctx: Quotes)(tp: qctx.reflect.TypeRepr): qctx.reflect.TypeRepr =
    import quotes.reflect._
    tp match
      case OrType(lhs, rhs) =>
        val nLhs = veryWiden(lhs)
        val nRhs = veryWiden(rhs)
        if (nLhs =:= nRhs) then nLhs
        else OrType(veryWiden(lhs), veryWiden(rhs))
      case other => tp.widen

  def ensureTyped(using qctx: Quotes)(term: qctx.reflect.Term, tp: qctx.reflect.TypeRepr): qctx.reflect.Term =
    import quotes.reflect._
    if (term.tpe =:= tp) then term
    else Typed(term, Inferred(tp))

  def safeShow(using Quotes)(t: quotes.reflect.Tree): String =
    try t.show
    catch
      case ex: Exception =>
        t.toString

  def findAllOwnersIn(using Quotes)(tree: quotes.reflect.Tree): Map[Int, quotes.reflect.Tree] =
    import quotes.reflect._
    import util._
    val search = new TreeAccumulator[Map[Int, Tree]] {

      def foldTree(x: Map[Int, Tree], tree: Tree)(owner: Symbol): Map[Int, Tree] =
        foldOverTree(x, tree)(owner)

      override def foldOverTree(x: Map[Int, Tree], tree: Tree)(owner: Symbol): Map[Int, Tree] = {
        tree match
          case t: Definition =>
            val maybeOwner = t.symbol.maybeOwner
            if (maybeOwner != Symbol.noSymbol && maybeOwner != owner) {
              x.get(maybeOwner.hashCode) match
                case Some(other) => x
                case None        => x.updated(maybeOwner.hashCode, t)
            } else
              x
          case TypedOrTest(expr, tpt) =>
            // workarround against https://github.com/lampepfl/dotty/issues/14393
            foldTree(foldOverTree(x, expr)(owner), tpt)(owner)
          case _ =>
            super.foldOverTree(x, tree)(owner)
      }
    }
    search.foldTree(Map.empty, tree)(Symbol.spliceOwner)

  def findDefinitionWithoutSymbol(using Quotes)(tree: quotes.reflect.Tree): Option[quotes.reflect.Tree] =
    import quotes.reflect._
    import util._
    val search = new TreeAccumulator[Option[Tree]] {

      def foldTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] =
        if (x.isDefined) x
        else foldOverTree(x, tree)(owner)

      override def foldOverTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] = {
        tree match
          case t: Definition =>
            if (t.symbol == Symbol.noSymbol) {
              Some(t)
            } else
              // searh deep
              // x
              super.foldOverTree(x, t)(owner)
          case _ =>
            super.foldOverTree(x, tree)(owner)
      }
    }
    search.foldTree(None, tree)(Symbol.spliceOwner)

  class DefinitionWithIncorrectOwner(using quotes: Quotes)(
      tree: quotes.reflect.Definition,
      owner: quotes.reflect.Symbol,
      expectedOwner: quotes.reflect.Symbol,
      contextOwner: quotes.reflect.Symbol
  ) {
    def show: String =
      s"Incorrect owner for ${tree.symbol}(${tree.symbol.hashCode}): real owner $owner(${owner.hashCode()}), expected owner: ${expectedOwner}(${expectedOwner.hashCode}, contextOwner: ${contextOwner}(${contextOwner.hashCode})"
  }

  def findSubtermWithIncorrectOwner(using
      Quotes
  )(currentOwner: quotes.reflect.Symbol, tree: quotes.reflect.Tree): Option[DefinitionWithIncorrectOwner] =
    import quotes.reflect._
    import util._
    val search = new TreeAccumulator[(Symbol, Option[DefinitionWithIncorrectOwner])] {

      def foldTree(x: (Symbol, Option[DefinitionWithIncorrectOwner]), tree: Tree)(
          owner: Symbol
      ): (Symbol, Option[DefinitionWithIncorrectOwner]) =
        if (x._2.isDefined) x
        else foldOverTree(x, tree)(owner)

      def checkOwner(sym: Symbol, owner: Symbol): Boolean =
        (sym.maybeOwner == owner)

      override def foldOverTree(x: (Symbol, Option[DefinitionWithIncorrectOwner]), tree: Tree)(
          owner: Symbol
      ): (Symbol, Option[DefinitionWithIncorrectOwner]) = {
        tree match
          case t: Definition =>
            if (checkOwner(t.symbol, owner)) {
              // searh deep
              super.foldOverTree((t.symbol, None), t)(owner)
            } else {
              (x._1, Some(new DefinitionWithIncorrectOwner(t, t.symbol.maybeOwner, x._1, owner)))
            }
          case _ =>
            super.foldOverTree(x, tree)(owner)
      }
    }
    search.foldTree((currentOwner, None), tree)(Symbol.spliceOwner)._2

  // used for debugging instrumentation
  def dummyMapper(using Quotes)(t: quotes.reflect.Term, owner: quotes.reflect.Symbol): Boolean =
    import quotes.reflect._
    var wasError = false
    val checker = new TreeMap() {

      override def transformTerm(tree: Term)(owner: Symbol): Term =
        try {
          super.transformTerm(tree)(owner)
        } catch {
          case ex: Throwable =>
            if (!wasError)
              ex.printStackTrace()
              wasError = true
            throw ex
        }

    }
    checker.transformTerm(t)(owner)
    wasError

  // workarround on bug, when changeowner does not change owner of definition, if this definietion
  //   is not inside some otjer tree.
  def reallyChangeOwner(using Quotes)(tree: quotes.reflect.Tree, owner: quotes.reflect.Symbol): quotes.reflect.Tree =
    import quotes.reflect.*
    tree match
      case td: Definition =>
        if (td.symbol.maybeOwner != Symbol.noSymbol && td.symbol.maybeOwner != owner) then
          val tmpBlock = Block(td :: Nil, '{}.asTerm)
          tmpBlock.changeOwner(owner) match
            case Block(x :: Nil, other) => x
            case _ =>
              throw MacroError(s"imposible: changeOwner changed structure of block ${tmpBlock}", tmpBlock.asExpr)
        else td
      case other =>
        other.changeOwner(owner)

  def prependStatementToBlock(using Quotes)(st: quotes.reflect.Statement, block: quotes.reflect.Block): quotes.reflect.Block = {
    import quotes.reflect.*
    val retval = block match
      case lambda @ Lambda(params, body) =>
        Block(st :: Nil, lambda)
      case _ =>
        block.expr match
          case Closure(_, _) =>
            Block(st :: Nil, block)
          case _ =>
            Block(st :: block.statements, block.expr)
    // if (false) {   TODO: pass debug-level
    //  dummyMapper(retval,Symbol.spliceOwner)
    // }
    retval
  }

  def prependStatementsToBlock(using
      Quotes
  )(sts: List[quotes.reflect.Statement], block: quotes.reflect.Block): quotes.reflect.Block = {
    import quotes.reflect.*
    val retval = block.expr match
      case Closure(_, _) =>
        Block(sts, block)
      case _ =>
        Block(sts ++: block.statements, block.expr)
    // if (true) {  TODO: pass debug-level
    //  dummyMapper(retval,Symbol.spliceOwner)
    // }
    retval
  }

  def inMonadOrChild[F[_]: Type](using Quotes)(te: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    te.widen.asType match
      case '[F[r]] if te.widen <:< TypeRepr.of[F[r]] =>
        Some(TypeRepr.of[r])
      case _ =>
        val bc = te.baseClasses
        if (bc.isEmpty) then None
        else
          TypeRepr
            .of[F]
            .classSymbol
            .flatMap(fs =>
              if (bc.contains(fs)) {
                Some(te.baseType(fs))
              } else {
                // TODO: search up, add tracking  F-bound to prevent infinite loop.
                None
              }
            )
  }

  def prependStatementsToTerm(using
      Quotes
  )(statements: List[quotes.reflect.Statement], term: quotes.reflect.Term): quotes.reflect.Block = {
    import quotes.reflect.*
    term match
      case lambda @ Lambda(_, _) =>
        Block(statements, lambda)
      case block @ Block(_, _) =>
        prependStatementsToBlock(statements, block)
      case _ =>
        Block(statements, term)
  }

  def lambdaBodyOwner(using Quotes)(lambda: quotes.reflect.Term): quotes.reflect.Symbol = {
    import quotes.reflect.*
    lambda match
      case Block((d: DefDef) :: Nil, Closure(_, _)) => d.symbol
      case Inlined(call, binding, body)             => lambdaBodyOwner(body)
      case _                                        => Symbol.noSymbol
  }

  /** @param fun
    *   expression, part of (Apply(fun, args)). Extract function symbols from carried applications: Apply( ... Apply(fun1, args1),
    *   ..., argsN) => fun1
    * @return
    *   extracted function symbol, (or just fun.symbol is this is not a carried application).
    */
  def extractCarriedFunSym(using Quotes)(fun: quotes.reflect.Term): quotes.reflect.Symbol = {
    import quotes.reflect.*
    fun match
      case Block((d: DefDef) :: Nil, Closure(_, _)) => d.symbol
      case Inlined(call, binding, body)             => extractCarriedFunSym(body)
      case TypeApply(Select(obj, "apply"), targs) if obj.tpe.widen.isFunctionType || obj.tpe.widen.isContextFunctionType =>
        extractCarriedFunSym(obj)
      case Select(obj, "apply") if obj.tpe.widen.isFunctionType || obj.tpe.widen.isContextFunctionType =>
        extractCarriedFunSym(obj)
      case _ =>
        fun.symbol
  }
