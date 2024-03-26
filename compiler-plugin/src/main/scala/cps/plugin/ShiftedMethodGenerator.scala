package cps.plugin

import scala.annotation.tailrec
import dotty.tools.dotc.*
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.util.SrcPos
import core.{ Names, Types, * }
import core.Names.*
import core.Contexts.*
import core.Constants.*
import core.Annotations.*
import core.Decorators.*
import core.Symbols.*
import core.Types.*
import ast.tpd.*
import transform.{ Erasure, Inlining, Pickler, PruneErasedDefs }
import plugins.*


object ShiftedMethodGenerator {




  def findShiftedParam(f: DefDef)(using Context): Option[Type] = {


    def findShiftedParamInType(tp: Type): Option[Type] =
      tp.widen.dealias match
        case pt: PolyType =>
          findShiftedParamInType(pt.resultType)
        case mt: MethodType =>
          mt.paramInfos.find(p => isFunction(p)).orElse(findShiftedParamInType(mt.resultType))
        case tp@AppliedType(base, targs) =>
          if (defn.isFunctionType(tp) || defn.isContextFunctionType(tp)) {
            val params = targs.dropRight(1)
            params.find(p => isFunction(p)).orElse(findShiftedParamInType(targs.last))
          } else {
            None
          }
        case _ =>
          None

    findShiftedParamInType(f.tpe.widen)

  }

  def generateShiftedMethod(md: DefDef)(using Context): Option[DefDef] = {
    println(s"generateShiftedMethod: ${md.symbol}")
    val shiftedType = ShiftedMethodGenerator.shiftedFunctionPolyType(md)
    println(s"shiftedTypeAsTree: ${shiftedType}")
    println(s"shiftedType: ${shiftedType.show}")

    val shiftedFunName    = (md.symbol.name.debugString + "_async").toTermName
    val newFunSymbol  = Symbols.newSymbol(md.symbol.owner, shiftedFunName, md.symbol.flags | Flags.Synthetic, shiftedType)
    val shiftedParams = selectHighOrderParamss(md.paramss)
    val newMethod =
      DefDef(
        newFunSymbol,
        newParamss => {
          println(s"newParamsDefDef=$newParamss")
          val paramssMap = buildParamssMap(md.tpe.widen, md.paramss, newParamss)
          // create new func body
          val transformedRhs = transformFunсBody(md.rhs, shiftedParams, newParamss)(using summon[Context].withOwner(newFunSymbol))
          // create new paramss
          TransformUtil.substParamsMap(transformedRhs, paramssMap).changeOwner(md.symbol, newFunSymbol)
        }
      )
    println(s"created newMethod ${newMethod.show}")
    Some(newMethod)
  }



  def shiftedFunctionPolyType(f: DefDef)(using Context): Type = {
    f.tpe.widen match
      case pt: PolyType =>
        println(s"polyType detected, pt=${pt.show}")
        println(s"polyType detected, pt.resType=${pt.resType.show}")

        val typeParamsNames = pt.paramNames.map(_.toTypeName)
          val fBound = TypeBounds(defn.NothingType, HKTypeLambda.any(1))
          //val cBound = TypeBounds(defn.NothingType, AppliedType(
          //  Symbols.requiredClassRef("cps.CpsMonadContext"),
          //  List(pt.newParamRef(0))
          //))


          PolyType(paramNames = List("F_SHIFT".toTypeName)++typeParamsNames)(
            npt => fBound :: {
              val newRefs = pt.paramInfos.zipWithIndex.map((p,i) => npt.newParamRef(i+1))
              pt.paramInfos.map(_.substParams(pt,newRefs).asInstanceOf[TypeBounds])
            },
            npt => pt.resType match
                    case  mt: MethodType =>
                       val newRefs = pt.paramInfos.zipWithIndex.map((p,i) => npt.newParamRef(i+1))        
                       shiftedFunctionMethodType(mt, npt.newParamRef(0) ).substParams(pt,newRefs)
                    case _ =>
                        throw CpsTransformException(s"Expected MethodTyoe in PolyType, we have ${f.tpe.widen.show} with ${pt.resType.show} instead method-tyoe", f.srcPos)
          )
      case mc: MethodType =>
          val fBound = TypeBounds(defn.NothingType, HKTypeLambda.any(1))
          //val cBound = TypeBounds(defn.NothingType, AppliedType(
          //  Symbols.requiredClassRef("cps.CpsMonadContext"),
          //  List(mc.newParamRef(0))
          //))
          PolyType(List("F_SHIFT".toTypeName /*, "C_SHIFT".toTypeName */ ))(
            pt => List(fBound /*, cBound*/),
            pt => shiftedFunctionMethodType(mc, pt.newParamRef(0) /*, pt.newParamRef(1) */ )
          )
      case _ =>
        throw CpsTransformException(s"Unsupported type of function: expected MethodTyoe or PolyType, we have ${f.tpe}", f.srcPos)
  }

  def shiftedFunctionMethodType(mt:MethodType, ftp: Type /*, ctp: Type*/)(using Context): Type = {
    val (shiftedParams, nHoParams) = shiftParamTypes(mt.paramInfos, 0, ftp /*, ctp*/)
    // create extra parameter with monad
    //  note, that this can be a CpsTryMonad if we want support try-catch
    MethodType(List("m".toTermName))(
      nt => List( Symbols.requiredClassRef("cps.CpsTryMonad").appliedTo(ftp)  ),
      nt => mt.derivedLambdaType(mt.paramNames,
                shiftedParams,
                shiftedFunctionReturnType(mt.resType, 0, ftp,/* ctp, */ nHoParams>0)
    ))
  }


  def shiftedFunctionReturnType(tp: Type, nesting: Int, ftp: Type, /*ctp: Type,*/ shiftedParamWasFound: Boolean )(using Context): Type = {
    tp.widen.dealias match
      case pt: PolyType =>
        // TODO:  adopt to dependend types.
        val nReturnType = shiftedFunctionReturnType(pt.resultType, nesting, ftp, /*ctp,*/ shiftedParamWasFound)
        pt.derivedLambdaType(resType = nReturnType)
      case mtp: MethodType =>
        val (shiftedParams, nHoParams) = shiftParamTypes(mtp.paramInfos, nesting, ftp /*, ctp*/)
        val nShiftedParamsWasFound = shiftedParamWasFound || nHoParams>0
        val nResType = shiftedFunctionReturnType(mtp.resType, nesting+1, ftp,/* ctp,*/ nShiftedParamsWasFound)
        mtp.derivedLambdaType(paramNames = mtp.paramNames, paramInfos = shiftedParams, resType = nResType)
      case tp@AppliedType(base,targs) =>
        if (defn.isFunctionType(tp) || defn.isContextFunctionType(tp)) {
          val params = targs.dropRight(1)
          val resType = targs.last
          val (shiftedParams, nHoParams) = shiftParamTypes(params, nesting, ftp)
          val nShiftedParamsWasFound = shiftedParamWasFound || nHoParams>0
          val nReturnType = shiftedFunctionReturnType(resType, nesting+1, ftp, /*ctp,*/ nShiftedParamsWasFound)
          defn.FunctionType(0).appliedTo(List(nReturnType))
        } else {
          CpsTransformHelper.cpsTransformedType(tp, ftp)
        }
      case _ =>
        // unchanges
        CpsTransformHelper.cpsTransformedType(tp, ftp)
  }


  def shiftParamTypes(paramTypes: List[Type], nesting: Int, ftp: Type)(using Context): (List[Type], Int) = {

    def shiftParam(tp:Type): Option[Type] = {
      if (defn.isFunctionType(tp)||defn.isContextFunctionType(tp)) {
        Some(CpsTransformHelper.cpsTransformedType(tp, ftp))
      } else {
        tp match
          case ExprType(rt) =>
            val nRt = CpsTransformHelper.cpsTransformedType(rt,ftp)
            Some(defn.FunctionType(0).appliedTo(List(nRt)))
          case _ =>
            // assume this is not a function type, so no need to shift
            None
      }
    }

    val s0 = (List.empty[Type], nesting)
    val (revS, n) = paramTypes.foldLeft(s0) { (s,e) =>
       val (shiftedParams, n) = s
       shiftParam(e) match
         case Some(ne) => (ne::shiftedParams, n+1)
         case None => (e::shiftedParams, n)
    }
    (revS.reverse, n)

  }

  def selectHighOrderParamss(paramss:List[List[TypeDef|ValDef]])(using Context): List[ValDef] = {

    def selectHightOrderParams(params:List[TypeDef|ValDef]): List[ValDef] =
      params.collect{ case v: ValDef if isFunction(v.tpt.tpe) => v }

    paramss.flatMap(selectHightOrderParams)

  }


  def buildParamssMap(tp: Type, oldParamss: List[ParamClause], newParamss: List[List[Tree]])(using Context): Map[Symbol, Tree] = {

    val (startFrom, startTo) =
      tp.widen.dealias match
        case pt: PolyType =>
          // we have added F[_] to type parameters, and parameter with monad.
          //(oldParamss.tail, newParamss.tail.tail)
          val newCorrespondent = newParamss.head.tail :: newParamss.tail.tail
          (oldParamss, newCorrespondent)
        case mt: MethodType =>
          (oldParamss, newParamss.tail.tail)

    // now it should be the same length
    var map = Map[Symbol, Tree]()
    startFrom.zip(startTo).foreach{ (from, to) =>
      from.headOption match
        case Some(h) =>
            h match
              case v: ValDef =>
                (from zip to).foreach{
                  case (oldV:ValDef, newV) =>
                    map = map.updated(oldV.symbol, newV)
                  case other =>
                    throw CpsTransformException(s"Internal error: expected (ValDef, Tree), we have $other", from.head.srcPos)
                }
              case _ =>
                //Type parameters, can skip
        case None =>
          throw CpsTransformException(s"Empty paramlist: impossible ", from.head.srcPos)
    }

    map
  }

  // Hight-level description of generation of async-shifted version of function
  //  val typeParams = if (haveTypeParams) paramss.head else Nil
  //  val normalArgs = if (haveTypeParams) args.tail else args
  //  Apply(
  //    TypeApply(
  //          ref(Symbols.requiredMetod("cps.cpsAsyncApply")),
  //          List[
  //            task - print and look.
  //            TypeTree(F[_],TypeParam,),
  //          ]
  //    ),
  //      Task:  add parameter to already existing function.
  //    List(
  //      ValDef(am, TypeTree(,,,)),
  //      ValDef(f, TypeTree(AppliedType(defn.ContextFunctionSymbol,))
  //    )
  //  )
  //   example:
  //   map[A,B](collection:List[A])(f: A => B): List[B] =  doSomething
  //   map$cps[F[_],C <: CpsMonadContext[F],A,B](am: CpsMonad.Aux[F,C])(collection:List[A])(f: A => B): F[List[B]] = {
  //      cpsAsyncApply[F,List[B],C](am,
  //           mt = ContextMethodType(....)
  //           Lambda(mt, tss => transformedBody(f) )
  //      )
  //   }
  //
  //   add type-params
  //      new typr of DefDef:  if its PolyType, - copy PolyType and add type parameter
  //                           MethodType - create PolyType with type-params and MethodType

  // original
  // class Functor[F[_]] {
  //    def map[A, B](f: A => B): F[A] => F[B]
  // }
  // expanded
  // class Functor[F <: Lambda1] {
  //    def map[A, B](f: A => B): F { type $hkArg$0 = A } # Apply  =>  F { type $hkArg$0 = B } # Apply
  // }

  //   transformedBody(f) = {
  //      Apply(f, ...)  =>  await[[F,result-typr-of-apply,F]](f, ....)
  //      or throw unimplemented instead await
  //

  //  DefDef( ....   rhs = cpsAsyncShift ....  )

  /**
   * transform rhs of the annotated function
   * @param tree - rhs of the function
   * @param functionParams - function params of the functions (we want to insert await-s for them)
   * @param newParamss - new parameters of the function
   * @param Context
   * @return
   */
  def transformFunсBody(
                         tree:          Tree,
                         functionParams:    List[ValDef],
                         newParamss:    List[List[Tree]]
  )(using Context): Tree =
    // val finalResType = tree.tpe.finalResultType
    // if isFunc(finalResType) then transformInnerFunction(tree)
    // else
    val newTypeParams = newParamss.head
    val typeParamF = newTypeParams.head
    //val typeParamC = newTypeParams.tail.head
    val newParams = newParamss.tail.head
    val paramAm    = newParams.head
    val methodName = "apply".toTermName
    //val monadTree = newParamss.head.head
    //val contextType = TermRef(monadTree.tpe,"Context".toTermName)
    val contextType = Select(paramAm, "Context".toTypeName).tpe
    val mtt        =
      ContextualMethodType(List("C".toTermName))(
        mt => List(contextType),
        mt => tree.tpe.widen
      )
    // f: C ?=> T
    val lambda     = Lambda(
      mtt,
      // get context
      contextParams => {
        val mapper = new TreeMap() {
          override def transform(tree: Tree)(using Context): Tree =
            tree match
              case Apply(TypeApply(Select(f @ Ident(fname), methodName), targs), args)
                  if (functionParams.exists(_.symbol == f.symbol)) =>
                insertAwait(tree, typeParamF, contextParams.head)
              case Apply(Select(f, methodName), args)
                  if (functionParams.exists(_.symbol == f.symbol)) =>
                insertAwait(tree, typeParamF, contextParams.head)
              case f: Ident if (functionParams.exists(_.symbol == f.symbol)) =>
                throw new CpsTransformException("Function is not invoked", tree.srcPos)
              case _ => super.transform(tree)
        }
        val body   = mapper.transform(tree)
        body
      }
    )
    // create cpsAsyncApply from awaited function
    Apply(
      TypeApply(
        ref(Symbols.requiredMethod("cps.plugin.cpsAsyncApply")),
        List(TypeTree(typeParamF.tpe), TypeTree(tree.tpe.widen), TypeTree(contextType))
      ),
      List(paramAm, lambda)
    )

  def insertAwait(tree: Tree, tF:Tree, monadCtx: Tree)(using
    Context
  ): Tree =
    Apply(
      Apply(
        TypeApply(
          // F[_],T, F[_]
          ref(Symbols.requiredMethod("cps.await")),
          List(tF, TypeTree(tree.tpe.widen), tF)
        ),
        List(tree)
      ),
      List(
        // using ctx: CpsMonadContext[F], conversion: CpsMonadConversion[F,G]
        monadCtx,
        TypeApply(
          ref(Symbols.requiredMethod("cps.CpsMonadConversion.identityConversion")),
          List(tF)
        )
      )
    ).withSpan(tree.span)

  /**
   * transform a function which is returned from the high-order annotated
   * function
   */
  // def transformInnerFunction(tree: Tree)(using Context): Tree =
  //   tree match
  //     // TODO: create a check for inline function
  //     case Block((innerFunc: DefDef) :: Nil, expr) => // unpack inner function
  //       Block(List(transformInnerFunction(innerFunc)), expr)
  //     case t: DefDef => // create a transformed copy of original inner function
  //       val rhs            = t.rhs
  //       val transformedRHS = transformInnerFunction(rhs)
  //       cpy.DefDef(t)(t.name, t.paramss, t.tpt, transformedRHS)
  //     case Block(stats, expr: Apply) => // transform inner function
  //       val newExpr = transformFunсBody(expr, List())
  //       Block(stats, newExpr)

  def checkApplicableForMakeCPS(tree: DefDef)(using Context): Either[String, Unit] =
    // check ValDef input params
    if !isHightOrderByArg(tree) then
      Left(
        "Object annotated with cps.plugin.annotation.makeCPS has to be a high-order function"
      )
    else if isFunction(tree.tpe.finalResultType) then
      Left("Unsupported type of function. The return type must not be a function")
    else Right(())

  def getHighOrderArgs(tree: DefDef)(using Context): List[ValDef] =
    val valDefs: List[ValDef] = filterParamsValDef(tree.paramss)
    val funcParams = valDefs.filter(p => isFunction(p.tpt.tpe))
    funcParams

  def isHightOrderByArg(tree: DefDef)(using Context): Boolean =
    // check ValDef input params
    val funcParams = getHighOrderArgs(tree)
    funcParams.nonEmpty

  def filterParamsValDef(params: List[ParamClause]): List[ValDef] =
    val ps = params.flatten[ValDef | TypeDef]
    ps.collect { case v: ValDef => v }

  def isFunction(tp: Type)(using Context): Boolean =
    val retval = tp.widen.dealias match
      case _: AppliedType
          if (defn.isFunctionType(tp) ||
            defn.isContextFunctionType(tp)) =>
        true
      case _: MethodType => true
      case _: PolyType => true
      case _ => false
    retval

}

