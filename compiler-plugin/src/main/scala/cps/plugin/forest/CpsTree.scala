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

/**
 * CpsTree -- transfomed element
 **/
sealed trait CpsTree {
  def tctx: TransformationContext
  
  /**
  * origin term, used for diagnostics.
  * TODO: only pos ?
  **/
  def origin: Tree

  /**
  * owner of origin
  **/
  def originOwner: Symbol

  /**
   * is this is a sync-tree ?
   **/
  def unpure(using Context): Option[Tree] 

  def transformed(using Context): Tree

  def transformedType(using Context): Type =
    CpsTransformHelper.cpsTransformedType(originType, tctx.monadType)
  
  def asyncKind: AsyncKind

  def isOriginEqSync(using Context): Boolean =
    unpure match
      case None => false
      case Some(unpureTerm) => (unpureTerm eq origin)

  def originType: Type = origin.tpe

  /**
   * let we have block {A; B}
   * cps({A;B}) = cps(A).appendInBlock(cps(B))
   **/
  def appendInBlock(next: CpsTree): CpsTree = {
    asyncKind match
      case AsyncKind.Sync =>
        SeqCpsTree(tctx,EmptyTree,originOwner,IndexedSeq(this),next)
      case AsyncKind.Async(_) =>
        next.asyncKind match
          case AsyncKind.Sync | _ : AsyncKind.AsyncLambda =>
            MapCpsTree(tctx,EmptyTree,originOwner, this,
                MapCpsTreeArgument(None,next)
            )
          case AsyncKind.Async(_) =>
            FlatMapCpsTree(tctx,EmptyTree,originOwner,this,
                FlatMapCpsTreeArgument(None,next)
            )
      case AsyncKind.AsyncLambda(_) =>
        // TODO: warning about unused lambda function
        SeqCpsTree(tctx,EmptyTree,originOwner,IndexedSeq(this),next)
  }

  /**
   *@returns a copy of cpstree with origin set to origin.
   **/
  def withOrigin(term:Tree):CpsTree 

   /**
   * apply runtime await.
   *  If this CpsTree is adync lambda f: x1...xn => F[y] transform
   *  one to f' = summon[RuntiemAwait[F].await(f(x1..xN))(monad,monad-context)]
   *  and return CpsTree with f' in unpure
   *  otherwise - unchanged or throw error
   *
   *  precondition:  kind  <: AsyncLambda(*)
   **/
  def applyRuntimeAwait(runtimeAwait: Tree)(using Context): CpsTree

  def select(name: Name, origin: Select, otpe: Type): CpsTree =
    SelectTypeApplyCpsTree(
      List(SelectTypeApplyCpsTree.OpSelect(origin)),
      this,
      origin,
      originOwner
    )
  
}

object CpsTree {

  def unchangedPure(tctx: TransformationContext, origin: Tree, owner: Symbol): PureCpsTree =
     PureCpsTree(tctx, origin, owner, origin)


  def pure(tctx: TransformationContext, origin: Tree, owner: Symbol, changed: Tree): PureCpsTree =
     PureCpsTree(tctx, origin, owner, changed)

  def impure(tctx: TransformationContext, origin: Tree, owner: Symbol, impure: Tree, internalKind:AsyncKind = AsyncKind.Sync): CpsTree =
     AsyncTermCpsTree(tctx, origin, owner, impure, internalKind)

  def unit(tctx: TransformationContext, owner: Symbol)(using Context): SyncCpsTree =
     UnitCpsTree(tctx, Literal(Constant(())), owner)

}


sealed trait SyncCpsTree extends CpsTree {

  def asyncKind: AsyncKind = AsyncKind.Sync

  def getUnpure(using Context): Tree

  def unpure(using Context): Option[Tree] = Some(getUnpure)

  override def transformed(using Context): Tree = {
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

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context): CpsTree =
    //not needed
    this

}


case class PureCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val term: Tree
) extends SyncCpsTree {

  override def unpure(using Context) = Some(term) 

  override def getUnpure(using Context) = term 

  override def appendInBlock(next: CpsTree): CpsTree = 
    SeqCpsTree(tctx,origin,originOwner,IndexedSeq(this),next)

  override def withOrigin(term:Tree): PureCpsTree =
    copy(origin=term) 

}

/**
 * seq, representing block which can be appended.
 * Only last element can be async here, 
 * others are always sync.
 **/
case class SeqCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  prevs: IndexedSeq[CpsTree],
  last: CpsTree
) extends CpsTree {

  override def asyncKind = last.asyncKind


  override def unpure(using Context) = {
    last.asyncKind match
      case AsyncKind.Sync =>
        val stats = prevs.map{ t =>
          t.unpure.get.changeOwner(t.originOwner,originOwner)
        }.toList
        Some(Block( stats, last.unpure.get.changeOwner(last.originOwner,originOwner) ))
      case _ =>
        None
  }

  override def transformed(using Context): Tree = {
    if (prevs.length == 0) then
      last.transformed
    else
      val tstats = prevs.map(t => t.transformed.changeOwner(t.originOwner,originOwner))
      val tlast = last.transformed.changeOwner(last.originOwner,originOwner)
      Block(tstats.toList,tlast)
  }
  
  // TODO: add span for error reporing
  override def appendInBlock(next: CpsTree): CpsTree = {
    last.asyncKind match
      case AsyncKind.Sync =>
        SeqCpsTree(tctx,origin,originOwner,prevs.appended(last),next)
      case AsyncKind.Async(_) =>
        SeqCpsTree(tctx,origin,originOwner,prevs,last.appendInBlock(next))
      case AsyncKind.AsyncLambda(_) =>
        throw CpsTransformException("Unused AsyncLambda ",origin.srcPos)
  }

  override def withOrigin(term:Tree): SeqCpsTree =
    copy(origin=term) 

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context): CpsTree =
    copy(last = last.applyRuntimeAwait(runtimeAwait))
  


}

sealed trait AsyncCpsTree extends CpsTree {

  def internalAsyncKind: AsyncKind

  override def asyncKind = AsyncKind.Async(internalAsyncKind)

  override def unpure(using Context) = None

  override def applyRuntimeAwait(runtimeAwait: Tree)(using Context): CpsTree =
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
    CpsTree.pure(tctx,origin,originOwner,tree)


}


case class AsyncTermCpsTree(
  tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val transformedTree: Tree,
  override val internalAsyncKind: AsyncKind
) extends AsyncCpsTree {


  override def asyncKind: AsyncKind =
    AsyncKind.Async(internalAsyncKind) 

  override def transformed(using Context): Tree =
    transformedTree

  override def appendInBlock(next: CpsTree): CpsTree = {
    next.asyncKind match
      case AsyncKind.Async(_) =>
        FlatMapCpsTree(tctx,origin,originOwner,this,FlatMapCpsTreeArgument(None,next))
      case _ =>
        MapCpsTree(tctx,origin,originOwner,this,MapCpsTreeArgument(None,next))
  }

  override def withOrigin(term:Tree): AsyncTermCpsTree =
    copy(origin=term) 
  

}


case class MapCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val mapSource: CpsTree,
  val mapFun: MapCpsTreeArgument  //  lambda function
) extends AsyncCpsTree {
   
  override def internalAsyncKind = 
    mapFun.body.asyncKind

  override def transformed(using Context): Tree = {
    mapSource.unpure match 
      case None =>
        val mapName = "map".toTermName
        Apply(
          Apply(
            TypeApply(
              Select(tctx.cpsMonadRef, mapName),
              List(TypeTree(mapSource.originType.widen), TypeTree(originType.widen))
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

  override def appendInBlock(next: CpsTree): CpsTree = {
    next.asyncKind match
      case AsyncKind.Async(_) =>
        FlatMapCpsTree(tctx,origin,originOwner,mapSource,
          FlatMapCpsTreeArgument(mapFun.optParam,mapFun.body.appendInBlock(next)))  
      case _ =>
        MapCpsTree(tctx,origin,originOwner,mapSource,
          MapCpsTreeArgument(mapFun.optParam,mapFun.body.appendInBlock(next)))
  }

  override def withOrigin(term:Tree): MapCpsTree =
    copy(origin=term) 


}


case class MapCpsTreeArgument(
    optParam: Option[ValDef],
    body:  CpsTree
)  {

  def makeLambda(mapCpsTree:MapCpsTree)(using Context): Block = {
    body.unpure match
      case None =>
        throw CpsTransformException("attempt use MapCpsTree with async argumemt. use FlatMapCpsTree instead",body.origin.srcPos)
      case Some(syncBody) =>
        val owner = mapCpsTree.originOwner
        val param = optParam.getOrElse{
          val sym = newSymbol(owner, "_unused".toTermName, Flags.EmptyFlags, 
                              mapCpsTree.mapSource.originType.widen, Symbols.NoSymbol)
          ValDef(sym,EmptyTree)
        }
        TransformUtil.makeLambda(List(param), body.originType.widen, owner, syncBody, body.originOwner)
  }  
  
}

case class FlatMapCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val flatMapSource: CpsTree,
  val flatMapFun: FlatMapCpsTreeArgument
) extends AsyncCpsTree {

  override def internalAsyncKind: AsyncKind =
    flatMapFun.body.asyncKind match
      case AsyncKind.Sync => AsyncKind.Sync
      case AsyncKind.Async(ak) => ak
      case AsyncKind.AsyncLambda(x) =>
        throw CpsTransformException("Invalid flatMap - result of function body should not be lambda",origin.srcPos)

  override def transformed(using Context): Tree = {
    val flatMapName = "flatMap".toTermName
    Apply(
      Apply(
        TypeApply(
          Select(tctx.cpsMonadRef, flatMapName),
          List(TypeTree(flatMapSource.originType), TypeTree(originType))
        ),
        List(flatMapSource.transformed)
      ),
      List(flatMapFun.makeLambda(this))
    ).withSpan(origin.span)
  }

  override def appendInBlock(next: CpsTree): CpsTree = {
    FlatMapCpsTree(tctx, origin, originOwner, flatMapSource, 
                        FlatMapCpsTreeArgument(flatMapFun.optParam, flatMapFun.body.appendInBlock(next)))
  }

  override def withOrigin(term:Tree): FlatMapCpsTree =
    copy(origin=term) 


}

case class FlatMapCpsTreeArgument(
   optParam: Option[ValDef], 
   body: CpsTree
) {

   def makeLambda(cpsTree: FlatMapCpsTree)(using Context): Block = {
    val owner = cpsTree.originOwner
    val param = optParam.getOrElse{
      val sym = newSymbol(owner, "_unused".toTermName, Flags.EmptyFlags, 
                          cpsTree.flatMapSource.originType.widen, Symbols.NoSymbol)
      ValDef(sym,EmptyTree)
    }
    val transformedBody = body.transformed(using summon[Context].withOwner(body.originOwner))
    TransformUtil.makeLambda(List(param),body.transformedType,owner,transformedBody, body.originOwner)
  }

}


/**
 *lambda without type-params and one argument list.
 **/
case class LambdaCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val originDefDef: DefDef,
  val cpsBody: CpsTree
)  extends CpsTree {

  if (originDefDef.paramss.length != 1) {
    throw new  CpsTransformException("Lambda function can have only one parameter list",origin.srcPos)
  }

  override def asyncKind = AsyncKind.AsyncLambda(cpsBody.asyncKind)

  override def unpure(using Context): Option[Tree] = {
    cpsBody.unpure match
      case None => None
      case Some(unpureBody) =>
        origin match
          case b:Block if b.stats.head eq originDefDef =>
            Some(origin)
          case _ =>
            val tpe = createUnshiftedType()
            val meth = Symbols.newAnonFun(originOwner,tpe)
            val closure = Closure(meth, tss => TransformUtil.substParams(unpureBody, originParams, tss.head)
                                                            .changeOwner(cpsBody.originOwner, meth)
                          )
            Some(closure)
  }
    

  override def transformed(using Context): Tree = {
    val tpe = createShiftedType()
    val meth = Symbols.newAnonFun(originOwner,tpe)
    // here cpsBody is received in other context
    // .  TODO:  check ownitu in cpsBody.transformed
    Closure(meth, tss => TransformUtil.substParams(cpsBody.transformed, originParams, tss.head).changeOwner(cpsBody.originOwner, meth))
  }

  override def appendInBlock(next: CpsTree): CpsTree = {
    SeqCpsTree(tctx,EmptyTree,originOwner,IndexedSeq(this),next)
  }

  override def withOrigin(term:Tree): LambdaCpsTree =
    copy(origin=term) 


  override def applyRuntimeAwait(runtimeAwait:Tree)(using Context): CpsTree =
    cpsBody.asyncKind match
      case AsyncKind.Sync => 
        // if we can restore origin fun, don't need apply awakt
        this
      case AsyncKind.Async(internal) =>
        val awaitMethod = Select(runtimeAwait,"await".toTermName)
        val tree = Apply(
          Apply(
            TypeApply(awaitMethod, List(TypeTree(cpsBody.originType.widen))),
            List(cpsBody.transformed)
          ),
          List(
            tctx.cpsMonadRef,
            tctx.cpsMonadContextRef
          )
        )
        CpsTree.pure(tctx,origin,originOwner,tree)
      case AsyncKind.AsyncLambda(internalAsync) =>
        ???
        
        


  private def originParams(using Context) = originDefDef.termParamss.head

  private def createShiftedType()(using Context): Type = {
    val params = originDefDef.termParamss.head
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpe)
    MethodType(paramNames)(
      x => paramTypes,
      x => CpsTransformHelper.cpsTransformedType(cpsBody.transformedType, tctx.monadType)
    )    
  }

  private def createUnshiftedType()(using Context): Type = {
    val params = originDefDef.termParamss.head
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpe)
    MethodType(paramNames)(
      x => paramTypes,
      x => cpsBody.originType
    )    
  }

}


/**
 * one Unit 
 **/
case class UnitCpsTree(override val tctx: TransformationContext, 
                       override val origin: Tree, 
                       override val originOwner: Symbol) extends SyncCpsTree {

    override def getUnpure(using Context): Tree = origin

    override def appendInBlock(next: CpsTree): CpsTree = next

    override def withOrigin(term:Tree): CpsTree =
      copy(origin = term)
  

}


case class BlockBoundsCpsTree(internal:CpsTree) extends CpsTree {

    override def tctx = internal.tctx
    override def origin = internal.origin
    override def originOwner = internal.originOwner

    override def unpure(using Context) = internal.unpure

    override def transformed(using Context) = internal.transformed

    override def asyncKind = internal.asyncKind



    override def withOrigin(term:Tree): CpsTree =
      BlockBoundsCpsTree(internal.withOrigin(term))

    override def applyRuntimeAwait(runtimeAwait:Tree)(using Context): CpsTree =
      BlockBoundsCpsTree(internal.applyRuntimeAwait(runtimeAwait))

  
}

case class SelectTypeApplyCpsTree(records: Seq[SelectTypeApplyCpsTree.Operation], 
                                  nested: CpsTree,  
                                  override val origin:Tree,
                                  override val originOwner: Symbol
                                ) extends CpsTree {

    override def tctx = nested.tctx

    override def asyncKind: AsyncKind = nested.asyncKind

    override def transformed(using Context): Tree = {
      nested.unpure match
        case None =>
          val paramSym = newSymbol(originOwner, "x".toTermName, Flags.EmptyFlags, 
                                   nested.originType.widen, Symbols.NoSymbol)
          val param = ValDef(paramSym, EmptyTree)
          //TODO:  we can not change owner in bodu,
          TransformUtil.makeLambda(List(param),originType.widen, originOwner,  prefixTerm(ref(paramSym)), ctx.owner)
        case Some(nestedTerm) =>
          if (nested.isOriginEqSync) then
            origin
          else 
            prefixTerm(nestedTerm)   
    } 

    override def unpure(using Context) = {
      nested.unpure map (prefixTerm _)
    }
  
    override def withOrigin(newOrigin: Tree) =
      copy(origin = newOrigin)

    override def applyRuntimeAwait(runtimeAwait:Tree)(using Context): CpsTree =
      copy(nested = nested.applyRuntimeAwait(runtimeAwait))
  

    override def select(name: Name, origin: Select, otpe: Type): CpsTree =
      copy(records = records.appended(SelectTypeApplyCpsTree.OpSelect(origin)))
      
         

    private def prefixTerm(nestedTerm: Tree)(using Context): Tree =
      records.foldLeft(nestedTerm){(s,e) => 
        e.prefixTerm(s)
      }

  

}

object SelectTypeApplyCpsTree {

   sealed trait Operation {
      def prefixTerm(term:Tree)(using Context): Tree
   }

   case class OpSelect(origin:Select) extends Operation {
      override def prefixTerm(term:Tree)(using Context): Tree =
        val sym = origin.symbol
        val ntpe = TermRef(term.tpe, origin.name.toTermName, sym.denot.asSeenFrom(term.tpe))
        Select(term,Types.TermRef(term.tpe,sym)).withType(ntpe).withSpan(origin.span)
   }

   case class OpTypeApply(origin:TypeApply) extends Operation {
      override def prefixTerm(term:Tree)(using Context): Tree =
        if (origin.args.isEmpty) then
          term
        else
          TypeApply(term,origin.args).withSpan(origin.span)
   }


}
