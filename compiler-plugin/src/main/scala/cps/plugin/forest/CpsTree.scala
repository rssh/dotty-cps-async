package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Constants.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import core.Names.*
import ast.tpd.*
import cps.plugin.*
import dotty.tools.dotc.ast.tpd

/**
 * CpsTree -- transfomed element
 *
 * Hierarchy:
 *  CpsTree
 *    |- SyncCpsTree
 *    |    |- PureCpsTree
 *    |    |- UnitCpsTree
 *    |- AsyncCpsTree
 *    |    |-  AsyncTermCpsTree
 *    |    | - MapCpsTree
 *    |    | - FlatMapCpsTree
 *    |- LambdaCpsTree
 *    |- OpaqueAsyncLambdaTermCpsTree
 *    |- SeqCpsTree
 *    |- BlockBoundsCpsTree
 *    |- SelectTypeApplyTypedCpsTree
 *    |- InlinedCpsTree(can-be-deleted)
 *    |- DefinitionCpsTree
 *    |- CallChainSubstCpsTree
 *
 **/
sealed trait CpsTree {
  
  /**
  * origin term, used for diagnostics.
  * TODO: only pos ?
  **/
  def origin: Tree

  /**
  * owner of term.
  *  At first this is the owner at origin, than can be changed 
  *  via changeOwner
  **/
  def owner: Symbol

  /**
   * is this is a sync-tree ?
   **/
  def unpure(using Context, CpsTopLevelContext): Option[Tree]

  def transformed(using Context, CpsTopLevelContext): Tree

  def transformedType(using Context, CpsTopLevelContext): Type =
    CpsTransformHelper.cpsTransformedType(originType, tctx.monadType)
  
  def asyncKind(using Context, CpsTopLevelContext): AsyncKind


  def isOriginEqSync(using Context, CpsTopLevelContext): Boolean =
    unpure match
      case None => false
      case Some(unpureTerm) => (unpureTerm eq origin)

  def originType(using Context): Type = origin.tpe

  def castOriginType(ntpe:Type)(using Context, CpsTopLevelContext): CpsTree

  /**
   * let we have block {A; B}
   * cps({A;B}) = cps(A).appendInBlock(cps(B))
   **/
  def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    asyncKind match
      case AsyncKind.Sync =>
        SeqCpsTree(EmptyTree,owner,IndexedSeq(this),next.changeOwner(owner))
      case AsyncKind.Async(_) =>
        next.asyncKind match
          case AsyncKind.Sync | _ : AsyncKind.AsyncLambda =>
            MapCpsTree(EmptyTree,owner, this,
                MapCpsTreeArgument(None,next)
            )
          case AsyncKind.Async(_) =>
            FlatMapCpsTree(EmptyTree,owner,this,
                FlatMapCpsTreeArgument(None,next)
            )
      case AsyncKind.AsyncLambda(_) =>
        // TODO: warning about unused lambda function
        SeqCpsTree(EmptyTree,owner,IndexedSeq(this),next.changeOwner(owner))
  }

  /**
   *@returns a copy of cpstree with origin set to origin.
   *TODO: remove
   **/
  def withOrigin(term:Tree):CpsTree 

  def changeOwner(newOwner: Symbol)(using Context): CpsTree

   /**
   * apply runtime await.
   *  If this CpsTree is adync lambda f: x1...xn => F[y] transform
   *  one to f' = summon[RuntiemAwait[F].await(f(x1..xN))(monad,monad-context)]
   *  and return CpsTree with f' in unpure
   *  otherwise - unchanged or throw error
   *  TODO: move to Lambda
   *
   *  precondition:  kind  <: AsyncLambda(*)
   **/
  def applyRuntimeAwait(runtimeAwait: Tree)(using Context, CpsTopLevelContext): CpsTree

  def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree =
    println(s"CpsTree.select,  origin.type=${origin.tpe.widen.show}")
    SelectTypeApplyTypedCpsTree(
      List(SelectTypeApplyTypedCpsTree.OpSelect(origin)),
      this,
      origin
    )

  def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree =
    SelectTypeApplyTypedCpsTree(
      List(SelectTypeApplyTypedCpsTree.OpTyped(origin)),
      this,
      origin
    )

  def typeApply(origin:TypeApply)(using Context, CpsTopLevelContext): CpsTree =
    SelectTypeApplyTypedCpsTree(
      List(SelectTypeApplyTypedCpsTree.OpTypeApply(origin)),
      this,
      origin
    )

  def show(using Context): String

  def tctx(using CpsTopLevelContext): CpsTopLevelContext =
    summon[CpsTopLevelContext]

}


/*

  Block(DefDef(....), ref(closure))

  Block(List(valDef,valDef), expr) 
  { v1=.., v2=..,  expr }

 

*/
object CpsTree {

  def unchangedPure(origin: Tree, owner: Symbol): PureCpsTree =
     PureCpsTree(origin, owner, origin)

  def pure(origin: Tree, owner: Symbol, changed: Tree): PureCpsTree =
     PureCpsTree(origin, owner, changed)

  def impure(origin: Tree, owner: Symbol, impure: Tree, internalKind:AsyncKind): CpsTree =
     AsyncTermCpsTree(origin, owner, impure, internalKind)

  def unit(owner: Symbol)(using Context): SyncCpsTree =
     UnitCpsTree(Literal(Constant(())), owner)

  def lambda(origin:Tree, owner: Symbol, ddef: DefDef, transformedBody: CpsTree): CpsTree =
     LambdaCpsTree(origin, owner, ddef, transformedBody)

  // TODO: add optional inpure
  def opaqueAsyncLambda(origin: Tree, owner: Symbol, changed: Tree, bodyKind: AsyncKind): OpaqueAsyncLambdaTermCpsTree =
      OpaqueAsyncLambdaTermCpsTree(origin, owner, changed, bodyKind)

}


sealed trait SyncCpsTree extends CpsTree  {

  override def asyncKind(using Context, CpsTopLevelContext): AsyncKind = AsyncKind.Sync

  def getUnpure(using Context, CpsTopLevelContext): Tree

  override def unpure(using Context, CpsTopLevelContext): Option[Tree] = Some(getUnpure)

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    val pureName = "pure".toTermName
    // TODO: setPos
    Apply(
      TypeApply(
        Select(tctx.cpsMonadRef,pureName),
        List(TypeTree(originType.widen))
      ),
      List(getUnpure)
    )
     .withSpan(origin.span)
     .withType(transformedType)
  }

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context, CpsTopLevelContext): CpsTree =
    //not needed
    this


}


case class PureCpsTree(
                        override val origin: Tree,
                        override val owner: Symbol,
                        val term: Tree
) extends SyncCpsTree {

  override def originType(using Context): Type =
    term.tpe

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree = {
    if (originType =:= ntpe) then
      this
    else
      // TODO: check origin <:< ntpe?
      PureCpsTree(origin, owner, Typed(term,TypeTree(ntpe)))
  }

  override def unpure(using Context, CpsTopLevelContext) = Some(term)

  override def getUnpure(using Context,CpsTopLevelContext) = term

  override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree =
    SeqCpsTree(origin,owner,IndexedSeq(this),next.changeOwner(owner))

  override def withOrigin(term:Tree): PureCpsTree =
    copy(origin=term) 

  override def changeOwner(newOwner: Symbol)(using Context) =
    if (origin eq term) then
      val nTerm = term.changeOwner(owner,newOwner)
      PureCpsTree(nTerm,newOwner,nTerm)
    else
      PureCpsTree(origin.changeOwner(owner,newOwner),newOwner,term.changeOwner(owner,newOwner))

  override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
    if (isOriginEqSync && (origin.qualifier eq this.origin)) {
      CpsTree.unchangedPure(origin, owner)
    } else {
      CpsTree.pure(origin,owner,Select(term,Types.TermRef(term.tpe,origin.symbol)))
      //super.select(origin)
    }
  }



  override def typed(originTyped: Typed)(using Context, CpsTopLevelContext): CpsTree = {
    if (isOriginEqSync && (originTyped.expr eq this.origin))
      val r = CpsTree.unchangedPure(originTyped, owner)
      if (owner eq originTyped.symbol.owner)
        r
      else
        r.changeOwner(originTyped.symbol.owner)
    else
      super.typed(originTyped)
  }



  override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree =
    if (isOriginEqSync && origin.fun.eq(this.origin))
      val r = CpsTree.unchangedPure(origin, owner)
      if (owner eq origin.symbol.owner)
        r
      else
        r.changeOwner(origin.symbol.owner)
    else
      super.typeApply(origin)


  override def show(using Context): String = {
      s"PureCpsTree(t=${term.show})"
  }    


}

/**
 * seq, representing block which can be appended.
 * Only last element can be async here, 
 * others are always sync.
 **/
case class SeqCpsTree(
                       override val origin: Tree,
                       override val owner: Symbol,
                       prevs: IndexedSeq[CpsTree],
                       last: CpsTree
) extends CpsTree {

  override def asyncKind(using Context, CpsTopLevelContext) = last.asyncKind

  override def originType(using Context): Type =
    last.originType

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree =
    last.castOriginType(ntpe)

  override def unpure(using Context, CpsTopLevelContext) = {
    if (prevs.isEmpty) then
      last.unpure
    else
      last.asyncKind match
        case AsyncKind.Sync =>
          val stats = prevs.map{ t =>
             t.unpure.get.changeOwner(t.owner,owner)
          }.toList
          Some(Block( stats, last.unpure.get.changeOwner(last.owner,owner) ))
        case AsyncKind.AsyncLambda(_) =>
          last.unpure.map(etaExpand)
        case _ =>
          None
  }



  override def transformed(using Context, CpsTopLevelContext): Tree = {
    if (prevs.length == 0) then
      last.transformed
    else
      asyncKind match
        case AsyncKind.AsyncLambda(_) =>
          etaExpand(last.transformed)
        case _ =>
          val tstats = prevs.map(t => t.unpure.get.changeOwner(t.owner,owner))
          val tlast = last.transformed.changeOwner(last.owner,owner)
          Block(tstats.toList,tlast)
  }

  private def etaExpand(tLast: Tree)(using Context, CpsTopLevelContext): Tree = {
    TransformUtil.methodTypeFromFunctionType(tLast.tpe.widen, last.origin.srcPos) match
      case Some(mt) =>
        val meth = Symbols.newAnonFun(owner, mt)
        val closure = Closure(meth, tss => {
           val stats = prevs.map(_.unpure.get).toList
           val lastTransformed = tLast match
             case Block(List(ddef),closure) =>
               Apply(Select(tLast, "apply".toTermName), tss.head)
             case _ =>
               Apply(tLast, tss.head)
           Block(stats, lastTransformed).changeOwner(owner,meth)
        })
        closure
      case None =>
        throw CpsTransformException(s"Can't convert ${tLast.tpe} to method type", last.origin.srcPos)
  }
  
  // TODO: add span for error reporing
  override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    last.asyncKind match
      case AsyncKind.Sync =>
        SeqCpsTree(origin,owner,prevs.appended(last),next)
      case AsyncKind.Async(_) =>
        SeqCpsTree(origin,owner,prevs,last.appendInBlock(next))
      case AsyncKind.AsyncLambda(_) =>
        throw CpsTransformException("Unused AsyncLambda ",origin.srcPos)
  }

  override def withOrigin(term:Tree): SeqCpsTree =
    copy(origin=term) 

  override def changeOwner(newOwner: Symbol)(using Context) =  {
    copy(owner=newOwner, prevs = prevs.map(_.changeOwner(newOwner)), last=last.changeOwner(newOwner))
  }

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context, CpsTopLevelContext): CpsTree =
    copy(last = last.applyRuntimeAwait(runtimeAwait))


  override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
    copy(last = last.select(origin))
  }

  override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
    copy(last = last.typed(origin))
  }

  override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree = {
    copy(last = last.typeApply(origin))
  }


  override def show(using Context): String = {
    val COMMA=","
    s"SeqCpsTree(${prevs.map(_.show).mkString(COMMA)},${last.show})"
  }

}

object SeqCpsTree {


}


sealed trait AsyncCpsTree extends CpsTree {

  def internalAsyncKind(using Context, CpsTopLevelContext): AsyncKind

  override def asyncKind(using Context, CpsTopLevelContext) = AsyncKind.Async(internalAsyncKind)

  override def unpure(using Context, CpsTopLevelContext) = None


  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context, CpsTopLevelContext): CpsTree =
    val tctx = summon[CpsTopLevelContext]
    val awaitMethod = Select(runtimeAwait,"await".toTermName)
    val tree = Apply(
      Apply(
        TypeApply(awaitMethod, List(TypeTree(originType.widen))),
        List(transformed)
      ),
      List(
        tctx.cpsMonadRef,
        tctx.cpsMonadContextRef
      )
    )
    CpsTree.pure(origin,owner,tree)


}


case class AsyncTermCpsTree(
                             override val origin: Tree,
                             override val owner: Symbol,
                             val transformedTree: Tree,
                             val vInternalAsyncKind: AsyncKind
) extends AsyncCpsTree {

  override def internalAsyncKind(using Context, CpsTopLevelContext): AsyncKind =
    vInternalAsyncKind

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree = {
    if (origin.tpe =:= ntpe) then
      this
    else  
      typed(Typed(origin,TypeTree(ntpe)))
  }

  override def asyncKind(using Context, CpsTopLevelContext): AsyncKind =
    AsyncKind.Async(internalAsyncKind) 

  override def transformed(using Context, CpsTopLevelContext): Tree =
    transformedTree

  override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    next.asyncKind match
      case AsyncKind.Async(_) =>
        FlatMapCpsTree(origin,owner,this,FlatMapCpsTreeArgument(None,next))
      case _ =>
        MapCpsTree(origin,owner,this,MapCpsTreeArgument(None,next))
  }

  override def withOrigin(term:Tree): AsyncTermCpsTree =
    copy(origin=term) 
  
  override def changeOwner(newOwner: Symbol)(using Context) = {
    copy(owner=newOwner,transformedTree = transformedTree.changeOwner(owner,newOwner))
  }  

  override def show(using Context): String = {
    s"AsyncTermCpsTree(t=${transformedTree.show})"
  }

}


case class MapCpsTree(
                       override val origin: Tree,
                       override val owner: Symbol,
                       val mapSource: CpsTree,
                       val mapFun: MapCpsTreeArgument //  lambda function
) extends AsyncCpsTree {

  override def originType(using Context): Type =
    mapFun.body.originType

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree = {
    copy(mapFun = mapFun.copy(body = mapFun.body.castOriginType(ntpe)))
  }

  override def internalAsyncKind(using Context, CpsTopLevelContext) =
    mapFun.body.asyncKind

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    val tctx = summon[CpsTopLevelContext]
    mapSource.unpure match 
      case None =>
        val mapName = "map".toTermName
        Apply(
          Apply(
            TypeApply(
              Select(tctx.cpsMonadRef, mapName),
              List(TypeTree(mapSource.originType.widen), TypeTree(TransformUtil.realWiden(originType)))
            ),
            List(mapSource.transformed)
          ),
          List(mapFun.makeLambda(this))
        ).withSpan(origin.span)
      case Some(unpureTerm) =>
        if (!(unpureTerm eq origin)) {
          Apply(mapFun.makeLambda(this) ,List(unpureTerm))
              .withSpan(origin.span)
        } else {
          origin
        }
  }

  override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    next.asyncKind match
      case AsyncKind.Async(_) =>
        FlatMapCpsTree(origin,owner,mapSource,
          FlatMapCpsTreeArgument(mapFun.optParam,mapFun.body.appendInBlock(next)))  
      case _ =>
        MapCpsTree(origin,owner,mapSource,
          MapCpsTreeArgument(mapFun.optParam,mapFun.body.appendInBlock(next)))
  }

  override def withOrigin(term:Tree): MapCpsTree =
    copy(origin=term) 

  override def changeOwner(newOwner: Symbol)(using Context) =
    // TODO: think about mapFun
    copy(owner=newOwner,mapSource = mapSource.changeOwner(newOwner))

  override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
    copy(mapFun = mapFun.copy(body = mapFun.body.select(origin)))
  }

  override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
    copy(mapFun = mapFun.copy(body = mapFun.body.typed(origin)))
  }

  override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree = {
    copy(mapFun = mapFun.copy(body = mapFun.body.typeApply(origin)))
  }


  override def show(using Context): String = {
    s"MapCpsTree(${mapSource.show},${mapFun.show})"
  }

}


case class MapCpsTreeArgument(
    optParam: Option[ValDef],
    body:  CpsTree
)  {

  def makeLambda(mapCpsTree:MapCpsTree)(using Context, CpsTopLevelContext): Block = {
    body.unpure match
      case None =>
        throw CpsTransformException("attempt use MapCpsTree with async argumemt. use FlatMapCpsTree instead",body.origin.srcPos)
      case Some(syncBody) =>
        val owner = mapCpsTree.owner
        val param = optParam match
          case Some(p) => p.changeOwner(p.symbol.owner, owner)
          case None =>
            val sym = newSymbol(owner, "_unused".toTermName, Flags.EmptyFlags, 
                              mapCpsTree.mapSource.originType.widen, Symbols.NoSymbol)
            ValDef(sym,EmptyTree)
        TransformUtil.makeLambda(List(param), body.originType.widen, owner, syncBody, body.owner)
  }  
  
  def show(using Context): String = {
    s"[${optParam.map(_.name).getOrElse("_unused")}=>${body.show}]"
  }

}

case class FlatMapCpsTree(
                           override val origin: Tree,
                           override val owner: Symbol,
                           val flatMapSource: CpsTree,
                           val flatMapFun: FlatMapCpsTreeArgument
) extends AsyncCpsTree {

  override def originType(using Context): Type =
    flatMapFun.body.originType

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree =
    copy(flatMapFun = flatMapFun.copy(body = flatMapFun.body.castOriginType(ntpe)))

  override def internalAsyncKind(using Context, CpsTopLevelContext): AsyncKind =
    flatMapFun.body.asyncKind match
      case AsyncKind.Sync => AsyncKind.Sync
      case AsyncKind.Async(ak) => ak
      case AsyncKind.AsyncLambda(x) =>
        throw CpsTransformException("Invalid flatMap - result of function body should not be lambda",origin.srcPos)

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    val tctx = summon[CpsTopLevelContext]
    // TODO: optimization: check that argument is pure and switch to map
    val flatMapName = "flatMap".toTermName
    Apply(
      Apply(
        TypeApply(
          Select(tctx.cpsMonadRef, flatMapName),
          List(TypeTree(flatMapSource.originType.widen), TypeTree(originType.widen))
        ),
        List(flatMapSource.transformed)
      ),
      List(flatMapFun.makeLambda(this))
    ).withSpan(origin.span)
  }

  override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    FlatMapCpsTree(origin, owner, flatMapSource,
                        FlatMapCpsTreeArgument(flatMapFun.optParam, flatMapFun.body.appendInBlock(next)))
  }

  override def withOrigin(term:Tree): FlatMapCpsTree =
    copy(origin=term) 

  override def changeOwner(newOwner: Symbol)(using Context) =
    copy(owner = newOwner, flatMapSource = flatMapSource.changeOwner(newOwner))


  override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
    // TODO:  check about asyncLambda in body
    println(s"FlatMap.select: origin.type.widen=${origin.tpe.widen.show}")
    println(s"FlatMap.select: origin.type=${origin.tpe.show}")
    val retval = copy(flatMapFun = flatMapFun.copy(body = flatMapFun.body.select(origin)))
    println(s"FlatMap.select: origin.type=${origin.tpe.show}")
    println(s"FlatMap.select: retval=${retval.show} ")
    retval
  }

  override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
    copy(flatMapFun = flatMapFun.copy(body = flatMapFun.body.typed(origin)))
  }

  override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree = {
    copy(flatMapFun = flatMapFun.copy(body = flatMapFun.body.typeApply(origin)))
  }


  override def show(using Context): String = {
    s"FlatMapCpsTree(${flatMapSource.show},${flatMapFun.show})"
  }  

}

case class FlatMapCpsTreeArgument(
   optParam: Option[ValDef], 
   body: CpsTree
) {

  def makeLambda(cpsTree: FlatMapCpsTree)(using Context, CpsTopLevelContext): Block = {
    val owner = cpsTree.owner
    val param = optParam.getOrElse{
      val sym = newSymbol(owner, "_unused".toTermName, Flags.EmptyFlags, 
                          cpsTree.flatMapSource.originType.widen, Symbols.NoSymbol)
      ValDef(sym,EmptyTree)
    }
    val transformedBody = body.transformed(using summon[Context].withOwner(body.owner), summon[CpsTopLevelContext])
    TransformUtil.makeLambda(List(param),body.transformedType,owner,transformedBody, body.owner)
  }

  def show(using Context):String =
    s"[${optParam.map(_.name).getOrElse("_unused")}] => ${body.show}"

}


/**
 *lambda without type-params and one argument list.
 **/
case class LambdaCpsTree(
                          override val origin: Tree,
                          override val owner: Symbol,
                          val originDefDef: DefDef,
                          val cpsBody: CpsTree
)  extends CpsTree {

  if (originDefDef.paramss.length != 1) {
    throw new  CpsTransformException("Lambda function can have only one parameter list",origin.srcPos)
  }

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree =
    if (defn.isFunctionType(ntpe) || defn.isContextFunctionType(ntpe)) then
      ntpe match
        case AppliedType(ntpfun, ntpargs) =>
          // TODO: check that params are the same.
          val nResType = ntpargs.last
          copy(cpsBody = cpsBody.castOriginType(nResType))
        case _ =>
          throw CpsTransformException(s"Can't cast lambda to type ${ntpe.show}: expected AppliedType but have ${ntpe}",origin.srcPos)
    else
      throw CpsTransformException(s"Can't cast lambda to type ${ntpe.show}: expected function type",origin.srcPos)

  override def asyncKind(using Context, CpsTopLevelContext) = AsyncKind.AsyncLambda(cpsBody.asyncKind)

  override def unpure(using Context, CpsTopLevelContext): Option[Tree] = {
    cpsBody.unpure match
      case None => None
      case Some(unpureBody) =>
        origin match
          case b:Block if (b.stats.head eq originDefDef) && cpsBody.isOriginEqSync =>
            Some(origin)
          case _ =>
            val tpe =  createUnshiftedType()
            println(s"LambdaCpsTree.unpure:  tpe=${tpe.show}  origin.tpe.widen=${origin.tpe.widen.show}")

            val meth = Symbols.newAnonFun(owner,tpe)
            val closure = Closure(meth, tss => TransformUtil.substParams(unpureBody, originParams, tss.head)
                                                            .changeOwner(cpsBody.owner, meth)
                          )
            val typedClosure = if (defn.isFunctionType(origin.tpe.widen) || tpe <:< origin.tpe.widen) then {
              closure
            } else {
              Typed(closure, TypeTree(origin.tpe.widen))
            }
            Some(typedClosure)
  }
    

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    val tpe = createShiftedType()
    val meth = Symbols.newAnonFun(owner,tpe)
    // here cpsBody is received in other context
    // .  TODO:  check ownitu in cpsBody.transformed
    Closure(meth, tss => TransformUtil.substParams(cpsBody.transformed, originParams, tss.head).changeOwner(cpsBody.owner, meth))
  }

  override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    SeqCpsTree(EmptyTree,owner,IndexedSeq(this),next.changeOwner(owner))
  }

  override def withOrigin(term:Tree): LambdaCpsTree =
    copy(origin=term) 

  override def changeOwner(newOwner: Symbol)(using Context) =
    copy(owner = newOwner)

  override def applyRuntimeAwait(runtimeAwait:Tree)(using Context, CpsTopLevelContext): CpsTree =
    cpsBody.asyncKind match
      case AsyncKind.Sync => 
        // if we can restore origin fun, don't need apply awakt
        this
      case AsyncKind.Async(internal) =>
        val awaitMethod = Select(runtimeAwait,"await".toTermName)
        val nBody = Apply(
          Apply(
            TypeApply(awaitMethod, List(TypeTree(cpsBody.originType.widen))),
            List(cpsBody.transformed)
          ),
          List(
            tctx.cpsMonadRef,
            tctx.cpsMonadContextRef
          )
        )
        println(s"applyRuntimeAwait:  runtimeAwait=${runtimeAwait.show}, nBody=${nBody.show}")
        val nDefDef =  ???
        val nLambda: Tree = ???
        CpsTree.pure(origin,owner,nLambda)
      case AsyncKind.AsyncLambda(internalAsync) =>
        ???


  override def show(using Context): String = {
    def showParamClause(params:ParamClause):String = {
       val ub = params.map(_.show).mkString(",")
       s"($ub)"
    }
    val originParams = originDefDef.paramss.map(showParamClause(_)).mkString("")
    s"LambdaCpsTree(${originParams},${cpsBody.show})"
  }
        
  private def originParams(using Context) = originDefDef.termParamss.head

  private def createShiftedType()(using Context, CpsTopLevelContext): Type = {
    val params = originDefDef.termParamss.head
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpe.widen)
    val retval = MethodType(paramNames)(
      x => paramTypes,
      x => cpsBody.transformedType.widen,
    )
    retval
  }

  private def createUnshiftedType()(using Context): Type = {
    val params = originDefDef.termParamss.head
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpe.widen)
    MethodType(paramNames)(
      x => paramTypes,
      x => cpsBody.originType.widen
    )    
  }

}

case class OpaqueAsyncLambdaTermCpsTree(
                          override val origin: Tree,
                          override val owner: Symbol,
                          val transformedTree: Tree,
                          val bodyKind: AsyncKind
                                       ) extends CpsTree {

  override def asyncKind(using Context, CpsTopLevelContext) = AsyncKind.AsyncLambda(bodyKind)

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree = {
    typed(Typed(origin,TypeTree(ntpe)))
  }

  override def unpure(using Context, CpsTopLevelContext): Option[Tree] = None

  override def transformed(using Context, CpsTopLevelContext): Tree = transformedTree


  override def applyRuntimeAwait(runtimeAwait: tpd.Tree)(using Context, CpsTopLevelContext): CpsTree = {
    val tpe = transformedTree.tpe.widen.dealias
    if (defn.isFunctionType(tpe) || defn.isContextFunctionType(tpe)) {
      val argTypes = tpe.argTypes
      val resultType = tpe.resultType
      val (mtRes, optParamType) = summon[CpsTopLevelContext].monadType match
        case HKTypeLambda(params, resType) =>
          // TODO: check that type-lambda have one argument and throw if nof
          (resType, params.headOption)
        case other => (other, None)
      val cntBase = tpe.baseType(mtRes.typeSymbol)
      if (!cntBase.exists) {
         throw CpsTransformException(s"Result type of ${tpe.show} should be wrapped into monad", origin.srcPos)
      }
      val unwrappedReturnType = cntBase match
        case AppliedType(base,cntArgs) =>
           cntArgs match
             case List(arg) => arg
             case other =>
               if (optParamType.isDefined) {
                 //  TODO:  unify AppliedType with HKTypeLambda.resultType
                 //   place of optParamType will be our unwrapped.
                 ???
               } else {
                 throw CpsTransformException(s"Can't unwrapped base type of ${cntBase.show} in the reuslt of ${tpe.show} ", origin.srcPos)
               }
        case other =>
           throw CpsTransformException(s"Expected that result type of AsyncLambda(${tpe.show}) is AppliedType, we have ", origin.srcPos)
      val mt = if (defn.isContextFunctionType(tpe)) {
        ContextualMethodType(paramNames = tpe.firstParamNames)(_ => argTypes, _ => unwrappedReturnType)
      } else {
        MethodType(paramNames = tpe.firstParamNames)(_ => argTypes, _ => unwrappedReturnType)
      }
      val nLambdaSym = Symbols.newAnonFun(owner,mt)
      val lambda = Closure(nLambdaSym,tss => {
        val nBody = Apply(
          Apply(
            TypeApply(runtimeAwait, List(TypeTree(unwrappedReturnType))),
            List(
              Apply(
                Select(transformedTree,"apply".toTermName),
                tss.head
              )
            )
          ),
          List(
            tctx.cpsMonadRef,
            tctx.cpsMonadContextRef
          )
        )
        nBody
      })
      CpsTree.pure(origin,owner,lambda)
    } else {
      throw CpsTransformException(s"Can't apply runtime await to ${tpe.show}: expected function type", origin.srcPos)
    }
  }

  override def changeOwner(newOwner: Symbol)(using Context): CpsTree = {
    copy(owner=newOwner, transformedTree = transformedTree.changeOwner(owner,newOwner))
  }

  override def withOrigin(term: tpd.Tree): CpsTree =
    copy(origin = term)

  override def show(using Context): String = {
    s"OpaqueAsyncLambdaTermCpsTree(${transformedTree.show})"
  }

  def toLambdaCpsTree(using Context, CpsTopLevelContext): LambdaCpsTree = {
    // check trivial casw where transformed tree is lambda itself
    transformedTree match
      case lambda@Block(List(ddef: DefDef), closure: Closure) if closure.meth.symbol == ddef.symbol  =>
        val Kind = CpsTransformHelper.asyncKindFromTransformedType(ddef.rhs.tpe.widen.dealias, summon[CpsTopLevelContext].monadType)
        LambdaCpsTree(origin, owner, ddef, CpsTree.impure(ddef.rhs, ddef.symbol, ddef.rhs, asyncKind))
      case _ =>
        TransformUtil.methodTypeFromFunctionType(transformedTree.tpe.widen.dealias, origin.srcPos) match
          case Some(mt) =>
            val sym = Symbols.newAnonFun(owner,mt)
            val lambda = DefDef(sym, tss => {
              val nBody = Apply(
                Select(transformedTree,"apply".toTermName),
                tss.head
              )
              nBody
            })
            val cpsBody = bodyKind match
              case AsyncKind.Sync => CpsTree.pure(EmptyTree,owner,lambda.rhs)
              case AsyncKind.Async(internalKind) => CpsTree.impure(EmptyTree,owner,lambda.rhs,internalKind)
              case AsyncKind.AsyncLambda(bodyKind) => CpsTree.opaqueAsyncLambda(EmptyTree,owner,lambda.rhs,bodyKind)
            LambdaCpsTree(origin, owner, lambda, cpsBody)
          case None =>
            throw CpsTransformException(s"Can't convert ${transformedTree.tpe.show} to method type", origin.srcPos)
  }



}


/**
 * one Unit 
 **/
case class UnitCpsTree(override val origin: Tree,
                       override val owner: Symbol) extends SyncCpsTree {

    override def getUnpure(using Context, CpsTopLevelContext): Tree = origin

    override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = next

    override def withOrigin(term:Tree): CpsTree =
      copy(origin = term)
  
    override def changeOwner(newOwner: Symbol)(using Context) =
      copy(owner=newOwner)

    override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
      throw new CpsTransformException("UnitCpsTree.select",origin.srcPos)
    }

    override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
      throw new CpsTransformException("UnitCpsTree.typed",origin.srcPos)
    }

    override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree = {
      throw new CpsTransformException("UnitCpsTree.typeApply",origin.srcPos)
    }

    override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree = {
      throw new CpsTransformException("UnitCpsTree.castOriginType", origin.srcPos)
    }

    override def show(using Context): String = {
      "UnitCpsTree"
    }  

}


case class BlockBoundsCpsTree(internal:CpsTree) extends CpsTree {


    override def origin = internal.origin
    override def originType(using Context) = internal.originType

    override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree =
      copy(internal=internal.castOriginType(ntpe))

    override def owner = internal.owner

    override def unpure(using Context, CpsTopLevelContext) = internal.unpure

    override def transformed(using Context, CpsTopLevelContext) = internal.transformed

    override def asyncKind(using Context, CpsTopLevelContext) = internal.asyncKind


    override def withOrigin(term:Tree): CpsTree =
      BlockBoundsCpsTree(internal.withOrigin(term))

    override def applyRuntimeAwait(runtimeAwait:Tree)(using Context, CpsTopLevelContext): CpsTree =
      BlockBoundsCpsTree(internal.applyRuntimeAwait(runtimeAwait))

    override def changeOwner(newOwner: Symbol)(using Context) =
      BlockBoundsCpsTree(internal.changeOwner(newOwner))

    override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
      BlockBoundsCpsTree(internal.select(origin))
    }

    override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
      BlockBoundsCpsTree(internal.typed(origin))
    }

    override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree = {
      BlockBoundsCpsTree(internal.typeApply(origin))
    }

    override def show(using Context):String = {
      s"BlockBoundsCpsTree(${internal.show})"
    }  

}

case class SelectTypeApplyTypedCpsTree(records: Seq[SelectTypeApplyTypedCpsTree.Operation],
                                  nested: CpsTree,  
                                  override val origin:Tree,
                                ) extends CpsTree {


    override def owner = nested.owner

    override def asyncKind(using Context, CpsTopLevelContext): AsyncKind = nested.asyncKind

     override def originType(using Context): Type =
       if (records.isEmpty) then
          nested.originType
       else
          records.last.originType

    override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree = {
      if (ntpe =:= originType) then
        this
      else
        typed(Typed(EmptyTree,TypeTree(ntpe)))
    }

    override def transformed(using Context, CpsTopLevelContext): Tree = {
      val retval = nested.asyncKind match
        case AsyncKind.Sync =>
          Apply(
            TypeApply(
              Select(tctx.cpsMonadRef, "pure".toTermName),
              List(TypeTree(originType.widen))
            ),
            List(prefixTerm(nested.unpure.get,AsyncKind.Sync))
          )
        case kind@AsyncKind.Async(internalKind) =>
          internalKind match
            case AsyncKind.Sync =>
              val paramSym = newSymbol(owner, "xSelectTypeApplyCpsTree".toTermName, Flags.EmptyFlags,
                                   nested.originType.widen, Symbols.NoSymbol)
              val param = ValDef(paramSym, EmptyTree)
              val lambda = TransformUtil.makeLambda(List(param), originType.widen, owner, prefixTerm(ref(paramSym), kind), ctx.owner)
              val mapName = "map".toTermName
              Apply(
                Apply(
                  TypeApply(
                    Select(tctx.cpsMonadRef, mapName),
                    List(TypeTree(nested.originType.widen), TypeTree(originType.widen))
                  ),
                  List(nested.transformed)
                ),
                List(lambda)
              ).withSpan(origin.span)
            case _ =>
              throw new CpsTransformException(s"SelectTypeApplyTypedCpsTree.transformed: unexpected nested internal kind:${internalKind}", origin.srcPos)
        case kind@AsyncKind.AsyncLambda(bodyType) =>
          prefixTerm(nested.transformed,kind)
      retval
    }

    override def unpure(using Context, CpsTopLevelContext) = {
      nested.unpure map (prefixTerm(_,AsyncKind.Sync))
    }
  
    override def withOrigin(newOrigin: Tree) =
      copy(origin = newOrigin)

    override def applyRuntimeAwait(runtimeAwait:Tree)(using Context, CpsTopLevelContext): CpsTree =
      copy(nested = nested.applyRuntimeAwait(runtimeAwait))
  

    override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree =
      copy(records = records.appended(SelectTypeApplyTypedCpsTree.OpSelect(origin)))

    override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree =
      copy(records = records.appended(SelectTypeApplyTypedCpsTree.OpTyped(origin)))

    override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree =
      copy(records = records.appended(SelectTypeApplyTypedCpsTree.OpTypeApply(origin)))

    override def changeOwner(newOwner:Symbol)(using Context) =
      copy(nested = nested.changeOwner(newOwner))  
         

    private def prefixTerm(nestedTerm: Tree, kind:AsyncKind)(using Context, CpsTopLevelContext): Tree =
      records.foldLeft(nestedTerm){(s,e) => 
        e.prefixTerm(s,kind)
      }

    override def show(using Context): String = {
      val sRecords = records.map(_.show).mkString(".")
      s"SelectTypeApplyCpsTree(${nested.show},${sRecords})"
    }  
  

}

object SelectTypeApplyTypedCpsTree {

   sealed trait Operation {
      def prefixTerm(term:Tree, kind: AsyncKind)(using Context, CpsTopLevelContext): Tree
      def show(using Context): String
      def originType: Type
   }

   case class OpSelect(origin:Select) extends Operation {

      override def prefixTerm(term:Tree, kind: AsyncKind)(using Context, CpsTopLevelContext): Tree =
        val sym = origin.symbol
        kind match
          case AsyncKind.Sync =>
            Select(term,Types.TermRef(term.tpe,sym)).withSpan(origin.span)
          case AsyncKind.Async(internalKind) =>
            // note, that here we are in map or flatMap, so one level
            if (internalKind != AsyncKind.Sync) then
              throw CpsTransformException(s"SelectTypeApplyTypedCpsTree.OpSelect: invalid shape for select: ${kind}", origin.srcPos)
            else
              Select(term,Types.TermRef(term.tpe,sym)).withSpan(origin.span)
          case AsyncKind.AsyncLambda(bodyKind) =>
            //  TODO: support for predefined set of functions.
            throw CpsTransformException(s"SelectTypeApplyTypedCpsTree.OpSelect: can't select AsyncLambda", origin.srcPos)

      override def show(using Context) = {
        origin.symbol.name.toString
      }

      override def originType: Type = origin.tpe

   }

   case class OpTypeApply(origin:TypeApply) extends Operation {

      override def prefixTerm(term:Tree, kind:AsyncKind)(using Context, CpsTopLevelContext): Tree = {
        if (origin.args.isEmpty) then
          term
        else
          TypeApply(term,origin.args).withSpan(origin.span)
      }

      override def show(using Context) = {
        val targs = origin.args.map(_.show).mkString(",")
        s"typeApply[$targs]"
      }

      override def originType: Type = origin.tpe

   }

   case class OpTyped(origin:Typed) extends Operation {

      override def prefixTerm(term:Tree, kind: AsyncKind)(using Context, CpsTopLevelContext): Tree = {
        kind match {
          case AsyncKind.AsyncLambda(bodyKind) =>
            val nTpt = CpsTransformHelper.cpsTransformedType(origin.tpt.tpe, summon[CpsTopLevelContext].monadType)
            Typed(term,TypeTree(nTpt)).withSpan(origin.span)
          case _ =>
            Typed(term,origin.tpt).withSpan(origin.span)
        }
      }

      override def show(using Context) = {
        s"typed[${origin.tpt.show}]"
      }

      override def originType: Type = origin.tpt.tpe

   }


}




//
//  disabled,  will be removed if we will not discover need in
//   walking into Inlined parts and opaqueAsyncLambda will be enoughf.
case class InlinedCpsTreeDisabled(override val origin:Inlined,
                          override val owner: Symbol,
                          bindings: List[MemberDef],  // transformed bindings.
                          expansion: CpsTree) extends CpsTree {

  override def asyncKind(using Context, CpsTopLevelContext) = expansion.asyncKind

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext) = ???

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    expansion.asyncKind match {
      case AsyncKind.Sync =>
        if (isOriginEqSync) {
          origin
        } else {
          val nExpansion = expansion.changeOwner(owner).unpure.get
          CpsTree.pure(origin,owner,Inlined(origin.call, bindings, nExpansion).withSpan(origin.span)).transformed
        }
      case AsyncKind.Async(nested) =>
        val nExpansion = expansion.changeOwner(owner).transformed
        CpsTree.impure(origin,owner,Inlined(origin.call, bindings, nExpansion).withSpan(origin.span), nested).transformed
      case AsyncKind.AsyncLambda(bodyKind) =>
        val nExpansion = expansion.changeOwner(owner).transformed
        Inlined(origin.call, bindings, nExpansion).withSpan(origin.span)
    }
  }

  override def unpure(using Context, CpsTopLevelContext) = {
    val retval = expansion.changeOwner(owner).unpure.map{ e =>
      Inlined(origin.call, bindings, e).withSpan(origin.span)
    }
    retval
  }

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context, CpsTopLevelContext): CpsTree = {
    val nExpansion = expansion.applyRuntimeAwait(runtimeAwait)
    nExpansion.asyncKind match {
      case AsyncKind.Sync =>
        CpsTree.pure(origin,owner,Inlined(origin.call, bindings, nExpansion.unpure.get).withSpan(origin.span))
      case AsyncKind.Async(nested) =>
        // impossible (or applying was failed)
        CpsTree.impure(origin,owner,Inlined(origin.call, bindings, nExpansion.transformed).withSpan(origin.span), nested)
      case AsyncKind.AsyncLambda(bodyKind) =>
        // impossible case, because applyRuntimeAwait should elimintate all AsyncLambda
        throw CpsTransformException(s"impossible case: AsyncLambda kind after applyRuntimeAwait", origin.srcPos)
    }
  }

  override def changeOwner(newOwner: Symbol)(using Context) =
    copy(owner = newOwner, expansion= expansion.changeOwner(newOwner))

  override def withOrigin(term: Tree):CpsTree =
    copy(origin = term.asInstanceOf[Inlined])

  override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
    copy(expansion = expansion.select(origin))
  }

  override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
    copy(expansion = expansion.typed(origin))
  }

  override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree = {
    copy(expansion = expansion.typeApply(origin))
  }

  override def show(using Context): String = {
    s"InlinedCpsTree(${bindings.map(_.show).mkString(",")}, ${expansion.show})"
  }

}

object InlinedCpsTree {


  case class TransformedBindingsResult(
                                      newBindings: List[MemberDef],
                                      substitutions: Map[Symbol,Tree]
                                      )


}


/**
 * Definition cps tree,
 *   Can be only in block and can't be last in block. (TODO: checked by BlockBoundsCpsTree)
 * @param origin
 * @param changed
 */
case class MemberDefCpsTree(
                              override val origin: Tree,
                              override val owner: Symbol,
                              definition: MemberDef) extends CpsTree {

  override def asyncKind(using Context, CpsTopLevelContext) = AsyncKind.Sync


  override def withOrigin(term: Tree): CpsTree = copy(origin = term)


  override def transformed(using Context, CpsTopLevelContext): Tree = {
    definition
  }

  override def unpure(using Context, CpsTopLevelContext) = {
    Some(definition)
  }

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext): CpsTree = {
    throw CpsTransformException(s"MemberDefCpsTree  can't be in position which require cast", origin.srcPos)
  }

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context, CpsTopLevelContext): CpsTree = this

  override def show(using Context): String = origin.show

  override def changeOwner(newOwner: Symbol)(using Context): CpsTree =
    copy(owner = newOwner, definition = definition.changeOwner(owner, newOwner))


}

case class CallChainSubstCpsTree(override val origin: Tree,
                                 override val owner: Symbol,
                                 call: CpsTree) extends CpsTree {

  call match
    case CallChainSubstCpsTree(_,_,_) =>
      throw CpsTransformException(s"CallChainSubstCpsTree can't be in position which require CallChainCpsTree", origin.srcPos)
    case _ =>

  override def asyncKind(using Context, CpsTopLevelContext) = finishChain().asyncKind

  override def withOrigin(term: Tree): CpsTree = copy(origin = term)

  override def castOriginType(ntpe: Type)(using Context, CpsTopLevelContext) = {
    finishChain().castOriginType(ntpe)
  }

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    finishChain().transformed
  }

  override def unpure(using Context, CpsTopLevelContext): Option[Tree] = {
    finishChain().unpure
  }

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context, CpsTopLevelContext): CpsTree = this

  override def show(using Context): String = s"CallChainCpsTree(${call.show})"

  override def changeOwner(newOwner: Symbol)(using Context): CpsTree =
    copy(owner = newOwner, call = call.changeOwner(newOwner))

  override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree = {
    CallChainSubstCpsTree(origin,owner, call.select(origin))
    //PureCpsTree(origin,owner,Select(call,origin.name).withSpan(origin.span))
    //throw CpsTransformException(s"impossible case: select after CallChainSubstCpsTree", origin.srcPos)
  }

  override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
    CallChainSubstCpsTree(origin,owner, call.typed(origin))
  }

  override def typeApply(origin: TypeApply)(using Context, CpsTopLevelContext): CpsTree = {
    CallChainSubstCpsTree(origin,owner,call.typeApply(origin))
    //throw CpsTransformException(s"impossible case: typeApply after CallChainSubstCpsTree", origin.srcPos)
  }


  def finishChain()(using Context, CpsTopLevelContext): CpsTree = {
     println(s"before _finishChain, call = ${call.show}")
     Thread.dumpStack()
     val finishChainSelect = call.select(Select(call.origin,"_finishChain".toTermName))
     val finishChainType = finishChainSelect.originType.widen
     if (finishChainType <:< defn.NothingType) then
        throw CpsTransformException(s"This chain call not support finish", finishChainSelect.origin.srcPos)
     else if (finishChainType.baseType(summon[CpsTopLevelContext].monadType.typeSymbol) != NoType) then
        finishChainSelect.asyncKind match
          case  AsyncKind.Sync =>
            CpsTree.impure(origin,owner,finishChainSelect.unpure.get,AsyncKind.Sync)
          case AsyncKind.Async(internalKind) =>
            val nSymbol = Symbols.newSymbol(owner, "x".toTermName, Flags.EmptyFlags, finishChainType, Symbols.NoSymbol)
            val nValDef = ValDef(nSymbol, EmptyTree)
            val nValRef = ref(nValDef.symbol)
            FlatMapCpsTree(origin,owner,finishChainSelect,FlatMapCpsTreeArgument(Some(nValDef),CpsTree.impure(origin,owner,nValRef,internalKind)))
          case AsyncKind.AsyncLambda(bodyKind) =>
            throw CpsTransformException(s"impossible case: finishChainType is Lambda", finishChainSelect.origin.srcPos)
     else if (finishChainType.baseType(Symbols.requiredClass("cps.runtime.CpsCallChainSubstAsyncShift"))!=NoType) then
       CallChainSubstCpsTree(origin,owner,finishChainSelect)
     else
       finishChainSelect
  }


}
