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
    val argTransformer = new TreeMap() {
            override def transformTerm(tree: Term)(owner: Symbol): Term =
               tree match
                 case Ident(name) => paramsMap.get(tree.symbol) match
                                        case Some(index) => Ref(indexedArgs(index).symbol)
                                        case _  => super.transformTerm(tree)(owner)
                 case _ => super.transformTerm(tree)(owner)
    }
    argTransformer.transformTerm(body)(owner)






