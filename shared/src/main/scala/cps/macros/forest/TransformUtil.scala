package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._


object TransformUtil:


  def find(using Quotes)(term: quotes.reflect.Term,
                       cond: quotes.reflect.Tree=> Option[quotes.reflect.Tree]) :Option[quotes.reflect.Tree] = {
     import quotes.reflect._
     import util._
     val search = new TreeAccumulator[Option[Tree]] {

        def foldTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] =
                 foldOverTree(x,tree)(owner)

        override def foldOverTree(x: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] = {
           if (x.isDefined)
             x
           else
             cond(tree) orElse super.foldOverTree(x,tree)(owner)
        }
     }
     search.foldTree(None,term)(Symbol.spliceOwner)
  }

  def containsAwait(using Quotes)(term: quotes.reflect.Term): Boolean =
    import quotes.reflect._
    find(term, {
           case v@Apply(TypeApply(id@Ident("await"),targs),args) =>
                         if (id.symbol.fullName == "cps.await") Some(v) else None
           case _ => None
         }).isDefined



  def createFunctionType(using Quotes)(argTypes: List[quotes.reflect.TypeRepr], 
                                       resultType: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect._
    val funSymbol = defn.FunctionClass(argTypes.size)
    val funTypeTree: TypeTree = TypeIdent(funSymbol)
    funTypeTree.tpe.appliedTo(argTypes.map(_.widen) :+ resultType.widen)

  
  def substituteLambdaParams(using qctx:Quotes)(oldParams:List[quotes.reflect.ValDef], 
                                                newParams: List[quotes.reflect.Tree], 
                                                body: quotes.reflect.Term, 
                                                owner: quotes.reflect.Symbol) : quotes.reflect.Term = 
    import quotes.reflect._
    val paramsMap = oldParams.zipWithIndex.map{case (tree,index)=>(tree.symbol,index)}.toMap
    val indexedArgs = newParams.toIndexedSeq

    //def lookupParamTerm(sym: Symbol): Option[Term] =
    //      paramsMap.get(sym).map(i => Ref(indexedArgs(i).symbol))
    val assoc: Map[Symbol, Tree] = paramsMap.map((k,i) => (k, Ref(indexedArgs(i).symbol)))
    changeSyms(assoc, body, owner)

  def changeSyms(using qctx:Quotes)(association: Map[qctx.reflect.Symbol,qctx.reflect.Tree], 
                                    body: quotes.reflect.Term, 
                                    owner: quotes.reflect.Symbol): quotes.reflect.Term =
    import quotes.reflect._

    // TODO: mege wirh changeSyms
    val argTransformer = new TreeMap() {

            def lookupParamTerm(sym: Symbol): Option[Term] =
                association.get(sym) match
                  case Some(paramTree) =>
                    paramTree match
                      case paramTerm: Term => Some(paramTerm)
                      case _ => 
                           throw MacroError(s"term expected for lambda param, we have ${paramTree}",body.asExpr)
                  case _ => None


            override def transformTree(tree: Tree)(owner: Symbol): Tree =
                tree match
                  case pattern: Bind =>
                    Bind.copy(pattern)(pattern.name, transformTree(pattern.pattern)(owner))
                  case _ =>
                    super.transformTree(tree)(owner)


            override def transformTerm(tree: Term)(owner: Symbol): Term =
               tree match
                 case Ident(name) => lookupParamTerm(tree.symbol) match
                                        case Some(paramTerm) => paramTerm
                                        case _  => super.transformTerm(tree)(owner)
                 case _ => super.transformTerm(tree)(owner)

            override def transformTypeTree(tree: TypeTree)(owner: Symbol):TypeTree =
               tree match
                 case Singleton(ref) => 
                       lookupParamTerm(ref.symbol) match
                          case Some(paramTerm) => Singleton(paramTerm)
                          case None => super.transformTypeTree(tree)(owner)
                 case a@Annotated(tp, annotation) =>
                          // bug in default TreeTransform, should process Annotated
                          Annotated.copy(a)(transformTypeTree(tp)(owner),transformTerm(annotation)(owner))
                 case i@Inferred() =>
                          Inferred(transformType(i.tpe)(owner))
                 case t:TypeSelect =>
                          TypeSelect.copy(t)(transformTerm(t.qualifier)(owner),t.name)
                 case _ => 
                          super.transformTypeTree(tree)(owner)

            def transformType(tp: TypeRepr)(owner: Symbol): TypeRepr =
               tp match
                 case ConstantType(c) => tp
                 case tref@TermRef(qual, name) =>
                         lookupParamTerm(tref.termSymbol) match
                           case Some(paramTerm) => paramTerm.tpe
                           case None => tp
                 case tp@TypeRef(internal, name) =>
                         internal match
                           case tr: TermRef =>
                              val ref = lookupParamTerm(tr.termSymbol).getOrElse(Ref(tr.termSymbol))
                              TypeSelect(ref,name).tpe
                           case _ =>
                            // we can't get inside, since it is
                            tp
                 case SuperType(thisTpe,superTpe) =>
                         SuperType(transformType(thisTpe)(owner),transformType(superTpe)(owner))
                 case Refinement(parent,name,info) =>
                         Refinement(transformType(parent)(owner),name,transformType(info)(owner))
                 case AppliedType(tycon, args) =>
                         transformType(tycon)(owner).appliedTo(args.map(x => transformType(x)(owner)))
                 case AnnotatedType(underlying, annot) =>
                         AnnotatedType(transformType(underlying)(owner), transformTerm(annot)(owner))
                 case AndType(rhs,lhs) => AndType(transformType(rhs)(owner),transformType(lhs)(owner))
                 case OrType(rhs,lhs) => OrType(transformType(rhs)(owner),transformType(lhs)(owner))
                 case MatchType(bound,scrutinee,cases) =>
                            MatchType(transformType(bound)(owner),transformType(scrutinee)(owner),
                                                        cases.map(x => transformType(x)(owner)))
                 case ByNameType(tp1) => ByNameType(transformType(tp1)(owner))
                 case ParamRef(x, index) => tp  //transform tp ?
                 case NoPrefix() => tp
                 case TypeBounds(low,hi) => TypeBounds(transformType(low)(owner),transformType(hi)(owner))
                 case _ => tp

    }
    argTransformer.transformTerm(body)(owner)

  /**
   * widen, which works over 'or' and 'and' types.
   *  (bug in dotty?)
   **/
  def veryWiden(using qctx: Quotes)(tp: qctx.reflect.TypeRepr): qctx.reflect.TypeRepr =
    import quotes.reflect._
    tp match
      case OrType(lhs,rhs) => val nLhs = veryWiden(lhs)
                              val nRhs = veryWiden(rhs)
                              if (nLhs =:= nRhs) then
                                  nLhs
                              else
                                  OrType(veryWiden(lhs),veryWiden(rhs))
      case other => tp.widen


  def ensureTyped(using qctx: Quotes)(term: qctx.reflect.Term, tp: qctx.reflect.TypeRepr): qctx.reflect.Term =
     import quotes.reflect._
     if (term.tpe =:= tp) then
         term
     else
         Typed(term, Inferred(tp))


  def safeShow(using Quotes)(t: quotes.reflect.Tree): String =
    try
      t.show
    catch
      case ex: Exception =>
         t.toString

  // used for debugging instrumentation
  def dummyMapper(using Quotes)(t: quotes.reflect.Term, owner: quotes.reflect.Symbol): Boolean =
     import quotes.reflect._
     var wasError = false
     val checker = new TreeMap() {


         override def transformTerm(tree: Term)(owner: Symbol): Term =
            try {
              super.transformTerm(tree)(owner)
            }catch{
              case ex: Throwable =>
                if (!wasError)
                  ex.printStackTrace() 
                  wasError = true
                throw ex
            }

     } 
     checker.transformTerm(t)(owner)
     wasError
     

