package cps.forest

import scala.quoted._

import cps._
import cps.misc._


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

    // TODO: mege wirh changeSyms
    val argTransformer = new TreeMap() {
            override def transformTerm(tree: Term)(owner: Symbol): Term =
               tree match
                 case Ident(name) => paramsMap.get(tree.symbol) match
                                        case Some(index) => Ref(indexedArgs(index).symbol)
                                        case _  => super.transformTerm(tree)(owner)
                 case _ => super.transformTerm(tree)(owner)
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
     

