package cps.plugin

import scala.annotation.tailrec
import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Decorators.*
import core.Symbols.*
import core.Types.*
import dotty.tools.dotc.typer.ProtoTypes
import util.SrcPos
import util.Spans.Span


object CpsTransformHelper {

  //def cpsTransformClassSymbol(using Context) =
  //  Symbols.requiredClass("cps.E.CpsTransform")

  def cpsMonadContextClassSymbol(using Context) =
      Symbols.requiredClass("cps.CpsMonadContext")

  def cpsDirectClassSymbol(using Context) =
    Symbols.requiredClass("cps.CpsDirect")


  def isCpsDirectType(tpe:Type, debug:Boolean=false)(using Context): Boolean = {
     val retval = tpe.dealias match
       case AppliedType(tycon, List(targ)) if (tycon.typeSymbol == cpsDirectClassSymbol) =>
         true
       case other =>
         other.baseType(cpsDirectClassSymbol) != NoType
     if (debug)
        println(s"isCpsDirectType(${tpe.show} [tree:${tpe}]) = ${retval}")
     retval
  }

  /**
   * Extract monad type from context function type.
   * TODO:  Type-lambda monads case,  now we think that this is F[_].
   *@param contextFunctionArgType is CpsDirect[F] or CpsMonadContext[F]
   *@param wrapperSymbol: naked symbol of wrapper. i.e. requiredClass("cps.CpsDirect") or requiredClass("cps.CpsMonadContext")
   *@return F
   **/
  def extractMonadType(contextFunctionArgType: Type, wrapperSymbol: Symbol, pos: SrcPos)(using Context): Type =
    contextFunctionArgType.dealias match
      case AppliedType(tycon, List(targ)) if (tycon.typeSymbol == wrapperSymbol) =>
             targ
      case other =>
             val cntBase = other.baseType(wrapperSymbol)
             if (cntBase != NoType) 
                cntBase match
                  case AppliedType(tycon, List(targ)) => targ
                  case _ =>
                    throw CpsTransformException(s"Can't extract monad from context-type: ${cntBase.show}", pos)
             else
                throw CpsTransformException(s"assumed that contect function type is ${wrapperSymbol}, but ${other.show} is not", pos)


  /**
   * Transform method type of function (...params) ?=> T  to  (...params) ?=> Cps[F,T]
   * @param lambdaTree - origin function
   * @param params - list of parameters of lambda function
   * @param body - function boldy
   * @param fType - type of F[_]
   * @return params ?=> F[T]
   */           
  def transformContextualLambdaType(lambdaTree: Tree, params: List[ValDef], body: Tree, fType:Type)(using Context): MethodType = {
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpt.tpe)
    ContextualMethodType(paramNames)(
      x => paramTypes,
      x => cpsTransformedType(body.tpe.widen, fType)
    )    
  }


  /**
   * ([T1,...Tn,R]   tranfrom to [T1,...Tn,CpsType[F,R]]
   *@param targs - type arguments of origin context function,
   **/
  def adoptResultTypeParam(targs:List[Type],fType:Type)(using Context):List[Type] = {
    @tailrec
    def advance(rest:List[Type], acc:List[Type]):List[Type] = {
       rest match
         case Nil  => acc  // impossible
         case last::Nil  => (cpsTransformedType(last,fType) :: acc).reverse
         case head::tail => advance(tail, head::acc)
    }
    advance(targs,Nil)
  }

  /**
   * CpsType[F[_],T] =
   *   F[T]  is T is not function type
   *   (X1..Xn) => CpsType[F, Y]  if T = (X1...Xn) => Y
   **/
  def cpsTransformedType(t:Type, fType:Type, debug: Boolean = false)(using Context): Type = {
    val retval = t match
      case AppliedType(funCn,params) =>
        if (debug) {
          println(s"cpsTransformedType, funCn = ${funCn.show}, isFunctionType=${defn.isFunctionType(funCn)}, isContextFunctionType=${defn.isContextFunctionType(funCn)}")
          println(s"cpsTransformedType, isContextFunctionType(t)=${defn.isContextFunctionType(t)}")
        }
        if (defn.isFunctionType(t) || defn.isContextFunctionType(t)) then
          val nParams = adoptResultTypeParam(params, fType)
          AppliedType(funCn, nParams)
        else
          decorateTypeApplications(fType).appliedTo(t.widen)
      case mt: MethodType =>
        if (mt.isContextualMethod)
          mt.derivedLambdaType(mt.paramNames, mt.paramInfos, cpsTransformedType(mt.resType, fType, debug))
          //ContextualMethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedType(mt.resType, fType, debug))
        else if (mt.isImplicitMethod)
          ImplicitMethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedType(mt.resType, fType, debug))
        else
          MethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedType(mt.resType, fType, debug))
      case pt: PolyType =>
        PolyType(pt.paramNames)(_ => pt.paramInfos, _ => cpsTransformedType(pt.resType, fType, debug))
      case _  =>
        decorateTypeApplications(fType).appliedTo(t.widen)
    if (debug) {
      println(s"cpsTransformedType: in = (${t.show}   [${t}]")
      println(s"cpsTransformedType: out = ${retval.show}   [${retval}]")
    }
    retval
  }

  def cpsTransformedErasedType(t:Type, fType:Type)(using Context): Type = {
    if (defn.isFunctionType(t) || defn.isContextFunctionType(t)) then
      // here we assume that  t is cps-transformed function, not F[(function-type)] which
      //  shoild be translated to F.
      t
    else
      t match
        case mt: MethodType =>
          if (mt.isContextualMethod)
            mt.derivedLambdaType(mt.paramNames, mt.paramInfos, cpsTransformedErasedType(mt.resType, fType))
            //ContextualMethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedErasedType(mt.resType, fType))
          else if (mt.isImplicitMethod)
            ImplicitMethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedErasedType(mt.resType, fType))
          else
            MethodType(mt.paramNames)(_ => mt.paramInfos, _ => cpsTransformedErasedType(mt.resType, fType))
        case _ =>
            // TODO: optimize
            val retval = TypeErasure.erasure(decorateTypeApplications(fType).appliedTo(t))
            println(s"eraded type application: ${retval.show},  is fType == ${retval == fType}, =:+= ${retval =:= fType}")
            retval
  }

  def asyncKindFromTransformedType(tpe: Type, ft: Type)(using Context): AsyncKind = {
    tpe match
      case AppliedType(tycon,args) if tycon =:= ft =>
        AsyncKind.Async(asyncKindFromTransformedType(args.head,ft))
      case AppliedType(tycon,args) if (defn.isFunctionSymbol(tycon.typeSymbol) || defn.isContextFunctionClass(tycon.typeSymbol)) =>
        AsyncKind.AsyncLambda(asyncKindFromTransformedType(args.last,ft))
      case mt: MethodType =>
        AsyncKind.AsyncLambda(asyncKindFromTransformedType(mt.resType,ft))
      case pt: PolyType =>
        AsyncKind.AsyncLambda(asyncKindFromTransformedType(pt.resType,ft))
      case other =>
        // TODO: use inferImplcit
        //summon[Context].typer.inferImplicit()
        ft match
          case HKTypeLambda(List(paramBound), resType) =>
            val withAny = ft.appliedTo(WildcardType)
            if (other <:< withAny) then
              // we can't determinate internal kind without running own type unfication,
              //  so,  approximate it with Sync
              AsyncKind.Async(AsyncKind.Sync)
            else
              AsyncKind.Sync
          case _ =>
             AsyncKind.Sync
  }

  /*
  def unwrappTypeFromMonad(ftExpr: Tree, ft: Type,  srcPos: SrcPos)(using Context): Type = {
    //TIDO: check
    //val (tl, tv) = ProtoTypes.constrained(ft,t)
    //tv.head
    t.widen.dealias match
      case AppliedType(tycon,args) if tycon =:= ft =>
        args.head
      case other =>
        throw CpsTransformException(s"can't extract type from ${other.show} as monad ${ft.show}", srcPos)

  }*/

  def findImplicitInstance(tpe: Type, span: Span)(using ctx:Context): Option[Tree] = {
    val searchResult = ctx.typer.inferImplicitArg(tpe,span)
    searchResult.tpe match
      case _ : typer.Implicits.SearchFailureType => None
      case _  => Some(searchResult)
  }
  
  def findRuntimeAwait(monadType: Type, span: Span)(using ctx:Context): Option[Tree] = {
      //TODO:  Problem: shows incorrect phase.
      val runtimeAwait = requiredClassRef("cps.CpsRuntimeAwait")
      val tpe = AppliedType(runtimeAwait, List(monadType))
      findImplicitInstance(tpe, span)
  }
  
  def findCpsThrowSupport(monadType:Type, span: Span)(using ctx:Context): Option[Tree] = {
      val cpsThrowSupport = requiredClassRef("cps.CpsThrowSupport")
      val tpe = AppliedType(cpsThrowSupport, List(monadType))
      findImplicitInstance(tpe, span)
  }

  def findCpsTrySupport(monadType: Type, span: Span)(using ctx:Context): Option[Tree] = {
      val cpsTrySupport = requiredClassRef("cps.CpsTrySupport")
      val tpe = AppliedType(cpsTrySupport, List(monadType))
      findImplicitInstance(tpe, span)
  }


}