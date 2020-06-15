package cps.forest.application

import scala.quoted._

import cps._
import cps.forest._
import cps.misc._


trait ApplyArgRecordScope[F[_], CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>
  
  import qctx.tasty._


  sealed trait ApplyArgRecord:
    def term: Term
    def index: Int
    def identArg: Term
    def isAsync: Boolean
    def hasShiftedLambda: Boolean
    def noOrderDepended: Boolean
    def useIdent: Boolean = isAsync || !noOrderDepended
    def isOrderDepended = !noOrderDepended
    def shift(): ApplyArgRecord
    def append(tree: CpsTree): CpsTree

  case class ApplyArgRepeatRecord(
       term: Repeated,
       index: Int,
       elements: List[ApplyArgRecord],
  ) extends ApplyArgRecord {
    override def useIdent: Boolean = (elements.exists(x => x.isAsync || x.isOrderDepended))
    override def identArg: Term = 
      if (useIdent)
          Repeated(elements.map(_.identArg),term.elemtpt)
      else if (hasShiftedLambda)
          Repeated(elements.map(_.identArg),shiftedLambdaTypeTree(term.elemtpt))
      else
          term
    override def isAsync = elements.exists(_.isAsync)
    override def hasShiftedLambda = elements.exists(_.hasShiftedLambda)
    override def noOrderDepended = elements.forall(_.noOrderDepended)
    override def shift() = copy(elements = elements.map(_.shift()))

    override def append(tree: CpsTree): CpsTree =
       if (elements.isEmpty)
         tree
       else 
         elements.foldRight(tree){(e,s) => 
           e match
            case _: ApplyArgNoPrecalcTermRecord =>
                  s
            case e1: ApplyArgPrecalcTermRecord =>
                  e.append(s)
            case _: ApplyArgLambdaRecord => s
            case _ => // incorrect warning
               throw MacroError("Impossible: repeated inside repeated",cpsCtx.patternCode)
         }
         

  }
  
  case class ApplyArgNoPrecalcTermRecord(
       term: Term,
       index: Int
  ) extends ApplyArgRecord
  {
     def isAsync = false
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg: Term = term
     def shift(): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree = tree
  }

  case class ApplyArgPrecalcTermRecord(
       term: Term,
       index: Int,
       termCpsTree: CpsTree,
       valDef: ValDef,
       ident: Term
  ) extends ApplyArgRecord
  {
     def isAsync = termCpsTree.isAsync
     def hasShiftedLambda: Boolean = false
     def noOrderDepended = termIsNoOrderDepended(term)
     def identArg: Term = ident
     def shift(): ApplyArgRecord = this
     override def append(tree: CpsTree): CpsTree =
        ValCpsTree(valDef, termCpsTree, tree)
  }


  case class ApplyArgLambdaRecord(
       term: Block,   // Lambda,  see coding of Lambda in Tasty Reflect.
       index: Int,
       cpsBody: CpsTree,
       shifted: Boolean
  ) extends ApplyArgRecord {

       def hasShiftedLambda: Boolean = cpsBody.isAsync

       def isAsync: Boolean = false

       def noOrderDepended: Boolean = true

       def identArg: Term = 
         if (hasShiftedLambda || shifted) 
            val params = term match
              case Lambda(params, body) => params
              case _ =>
                 throw MacroError(s"Lambda expexted, we have ${term.seal.show}",term.seal)
            val mt = term.tpe match 
              case MethodType(paramNames, paramTypes, resType) =>
                  shiftedMethodType(paramNames, paramTypes, resType)
              case ft@AppliedType(tp,tparams) =>
                  if (ft.isFunctionType) {
                      val paramTypes = tparams.dropRight(1).map(typeOrBoundsToType(_,false)) 
                      val resType = typeOrBoundsToType(tparams.last,true)
                      val paramNames = params.map(_.name)
                      shiftedMethodType(paramNames, paramTypes, resType)
                  } else {
                      throw MacroError(s"FunctionType expected, we have ${tp}", term.seal) 
                  }
              case other =>
                  // TODO: logging compiler interface instead println
                  println(s"MethodType expected, we have ${term.tpe}") 
                  println(s"term.show = ${term.show}") 
                  println(s"term.body = ${term}") 
                  println(s"mt = ${other}") 
                  throw MacroError(s"methodType expected for ${term.seal.show}, we have $other",term.seal)
            Lambda(mt, args => changeArgs(params,args,cpsBody.transformed))
         else 
            term

       def shift(): ApplyArgRecord = copy(shifted=true)

       def append(a: CpsTree): CpsTree = a

       private def changeArgs(params:List[ValDef], nParams:List[Tree], body: Term): Term =
         val association: Map[Symbol, Tree] = (params zip nParams).foldLeft(Map.empty){
           case (m, (oldParam, newParam)) => m.updated(oldParam.symbol, newParam)
         }
         val changes = new TreeMap() {
             override def transformTerm(tree:Term)(using ctx: Context):Term =
               tree match
                 case ident@Ident(name) => association.get(ident.symbol) match
                                            case Some(paramTree) =>
                                              paramTree match 
                                                case paramTerm: Term => paramTerm
                                                case _ =>
                                                 throw MacroError(s"term expected for lambda param, we ahave ${paramTree}",term.seal)
                                            case None => super.transformTerm(tree)
                 case _ => super.transformTerm(tree)
         }
         changes.transformTerm(body)
         
  }

  case class ApplyArgNamedRecord(term: NamedArg, name: String, nested: ApplyArgRecord ) 
     extends ApplyArgRecord {

       def index: Int = nested.index

       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def identArg: Term = NamedArg(name, nested.identArg)
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def append(a: CpsTree): CpsTree = nested.append(a)

  }

  case class ApplyArgInlinedRecord(origin: Inlined, nested: ApplyArgRecord ) 
     extends ApplyArgRecord {
       def index: Int = nested.index
       def term: Term =
             Inlined(origin.call, origin.bindings, nested.term)
       def hasShiftedLambda: Boolean = nested.hasShiftedLambda
       def isAsync: Boolean = nested.isAsync
       def noOrderDepended = nested.noOrderDepended
       def identArg: Term = nested.identArg
       def shift(): ApplyArgRecord = copy(nested=nested.shift())
       def append(a: CpsTree): CpsTree =
             val na = nested.append(a)
             if (na eq a)
                a
             else
                InlinedCpsTree(origin, na)
                

  }

 
  case class ApplyArgByNameRecord(term: Term, 
                                  index: Int, 
                                  cpsTree: CpsTree,
                                  shifted: Boolean) extends ApplyArgRecord:
    def identArg: Term = 
      if !shifted then
         term
      else
         val mt = MethodType(List())(_ => List(), _ => typeInMonad(term.tpe))
         Lambda(mt, args => cpsTree.transformed)
         
    def isAsync: Boolean = cpsTree.isAsync
    def hasShiftedLambda: Boolean = shifted
    def noOrderDepended: Boolean = true
    def shift() = copy(shifted = true)
    def append(tree: CpsTree): CpsTree = tree



  def termIsNoOrderDepended(x:Term): Boolean =
    x match {
      case Literal(_) => true
      case Ident(_) => if (x.symbol.isValDef) then
                         x.symbol.flags.is(Flags.Mutable)
                       else if x.symbol.isDefDef then
                         true
                       else if x.symbol.isBind then
                         true
                       else
                         false
      case _ => false  
    }


