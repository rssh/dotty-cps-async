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
 *    |- SeqCpsTree
 *    |- BlockBoundsCpsTree
 *    |- SelectTypeApplyTypedCpsTree
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
  
  def asyncKind: AsyncKind

  // TODO
  def asyncKindAccessor: CpsTreeKindAccessor =
    asyncKind match
      case AsyncKind.Sync =>
        new SyncCpsTreeAccessor {
          override def cpsTree = CpsTree.this
        }
      case k: AsyncKind.Async =>
        new AsyncCpsTreeAccessor {
          override def cpsTree = CpsTree.this
        }
      case k: AsyncKind.AsyncLambda =>
        new LambdaCpsTreeAccessor {
          override def cpsTree = CpsTree.this
        }


  def isOriginEqSync(using Context, CpsTopLevelContext): Boolean =
    unpure match
      case None => false
      case Some(unpureTerm) => (unpureTerm eq origin)

  def originType: Type = origin.tpe

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

  def impure(origin: Tree, owner: Symbol, impure: Tree, internalKind:AsyncKind = AsyncKind.Sync): CpsTree =
     AsyncTermCpsTree(origin, owner, impure, internalKind)

  def unit(owner: Symbol)(using Context): SyncCpsTree =
     UnitCpsTree(Literal(Constant(())), owner)

}


sealed trait SyncCpsTree extends CpsTree with SyncCpsTreeAccessor {

  def asyncKind: AsyncKind = AsyncKind.Sync
  override def asyncKindAccessor:CpsTreeKindAccessor = this

  def getUnpure(using Context, CpsTopLevelContext): Tree

  def unpure(using Context, CpsTopLevelContext): Option[Tree] = Some(getUnpure)

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

  // asyncKindAccessor methods
  override def cpsTree = this


}


case class PureCpsTree(
                        override val origin: Tree,
                        override val owner: Symbol,
                        val term: Tree
) extends SyncCpsTree {

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
      val r = CpsTree.unchangedPure(origin, owner)
      if (owner eq origin.symbol.owner) {
        r
      } else {
        r.changeOwner(origin.symbol.owner)
      }
    } else {
      super.select(origin)
    }
  }

  override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree = {
    if (isOriginEqSync && (origin.expr eq this.origin))
      val r = CpsTree.unchangedPure(origin, owner)
      if (owner eq origin.symbol.owner)
        r
      else
        r.changeOwner(origin.symbol.owner)
    else
      super.typed(origin)
  }


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

  override def asyncKind = last.asyncKind

  override def unpure(using Context, CpsTopLevelContext) = {
    last.asyncKind match
      case AsyncKind.Sync =>
        val stats = prevs.map{ t =>
          t.unpure.get.changeOwner(t.owner,owner)
        }.toList
        Some(Block( stats, last.unpure.get.changeOwner(last.owner,owner) ))
      case _ =>
        None
  }

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    if (prevs.length == 0) then
      last.transformed
    else
      val tstats = prevs.map(t => t.transformed.changeOwner(t.owner,owner))
      val tlast = last.transformed.changeOwner(last.owner,owner)
      Block(tstats.toList,tlast)
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
  
  override def show(using Context): String = {
    val COMMA=","
    s"SeqCpsTree(${prevs.map(_.show).mkString(COMMA)},${last.show})"
  }

}

sealed trait AsyncCpsTree extends CpsTree {

  def internalAsyncKind: AsyncKind

  override def asyncKind = AsyncKind.Async(internalAsyncKind)

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
                             override val internalAsyncKind: AsyncKind
) extends AsyncCpsTree {


  override def asyncKind: AsyncKind =
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
   
  override def internalAsyncKind = 
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

  override def internalAsyncKind: AsyncKind =
    flatMapFun.body.asyncKind match
      case AsyncKind.Sync => AsyncKind.Sync
      case AsyncKind.Async(ak) => ak
      case AsyncKind.AsyncLambda(x) =>
        throw CpsTransformException("Invalid flatMap - result of function body should not be lambda",origin.srcPos)

  override def transformed(using Context, CpsTopLevelContext): Tree = {
    val tctx = summon[CpsTopLevelContext]
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

  override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = {
    FlatMapCpsTree(origin, owner, flatMapSource,
                        FlatMapCpsTreeArgument(flatMapFun.optParam, flatMapFun.body.appendInBlock(next)))
  }

  override def withOrigin(term:Tree): FlatMapCpsTree =
    copy(origin=term) 

  override def changeOwner(newOwner: Symbol)(using Context) =
    copy(owner = newOwner, flatMapSource = flatMapSource.changeOwner(newOwner))

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

  override def asyncKind = AsyncKind.AsyncLambda(cpsBody.asyncKind)

  override def unpure(using Context, CpsTopLevelContext): Option[Tree] = {
    cpsBody.unpure match
      case None => None
      case Some(unpureBody) =>
        origin match
          case b:Block if b.stats.head eq originDefDef =>
            Some(origin)
          case _ =>
            val tpe = createUnshiftedType()
            val meth = Symbols.newAnonFun(owner,tpe)
            val closure = Closure(meth, tss => TransformUtil.substParams(unpureBody, originParams, tss.head)
                                                            .changeOwner(cpsBody.owner, meth)
                          )
            Some(closure)
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
case class UnitCpsTree(override val origin: Tree,
                       override val owner: Symbol) extends SyncCpsTree {

    override def getUnpure(using Context, CpsTopLevelContext): Tree = origin

    override def appendInBlock(next: CpsTree)(using Context, CpsTopLevelContext): CpsTree = next

    override def withOrigin(term:Tree): CpsTree =
      copy(origin = term)
  
    override def changeOwner(newOwner: Symbol)(using Context) =
      copy(owner=newOwner)  

    override def show(using Context): String = {
      "UnitCpsTree"
    }  

}


case class BlockBoundsCpsTree(internal:CpsTree) extends CpsTree {

    override def origin = internal.origin
    override def owner = internal.owner

    override def unpure(using Context, CpsTopLevelContext) = internal.unpure

    override def transformed(using Context, CpsTopLevelContext) = internal.transformed

    override def asyncKind = internal.asyncKind



    override def withOrigin(term:Tree): CpsTree =
      BlockBoundsCpsTree(internal.withOrigin(term))

    override def applyRuntimeAwait(runtimeAwait:Tree)(using Context, CpsTopLevelContext): CpsTree =
      BlockBoundsCpsTree(internal.applyRuntimeAwait(runtimeAwait))

    override def changeOwner(newOwner: Symbol)(using Context) =
      BlockBoundsCpsTree(internal.changeOwner(newOwner))  
  
    override def show(using Context):String = {
      s"BlockBoundsCpsTree(${internal.show})"
    }  

}

case class SelectTypeApplyTypedCpsTree(records: Seq[SelectTypeApplyTypedCpsTree.Operation],
                                  nested: CpsTree,  
                                  override val origin:Tree,
                                ) extends CpsTree {

    override def owner = nested.owner

    override def asyncKind: AsyncKind = nested.asyncKind

    override def transformed(using Context, CpsTopLevelContext): Tree = {
      nested.unpure match
        case None =>
          val paramSym = newSymbol(owner, "xSelectTypeApplyCpsTree".toTermName, Flags.EmptyFlags, 
                                   nested.originType.widen, Symbols.NoSymbol)
          val param = ValDef(paramSym, EmptyTree)
          //TODO:  we can not change owner in bodu,
          val lambda = TransformUtil.makeLambda(List(param),originType.widen, owner,  prefixTerm(ref(paramSym),false), ctx.owner)
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
        case Some(nestedTerm) =>
          if (nested.isOriginEqSync) then
            origin
          else 
            prefixTerm(nestedTerm, true)
    } 

    override def unpure(using Context, CpsTopLevelContext) = {
      nested.unpure map (prefixTerm(_,true))
    }
  
    override def withOrigin(newOrigin: Tree) =
      copy(origin = newOrigin)

    override def applyRuntimeAwait(runtimeAwait:Tree)(using Context, CpsTopLevelContext): CpsTree =
      copy(nested = nested.applyRuntimeAwait(runtimeAwait))
  

    override def select(origin: Select)(using Context, CpsTopLevelContext): CpsTree =
      copy(records = records.appended(SelectTypeApplyTypedCpsTree.OpSelect(origin)))

    override def typed(origin: Typed)(using Context, CpsTopLevelContext): CpsTree =
      copy(records = records.appended(SelectTypeApplyTypedCpsTree.OpTyped(origin)))

    override def changeOwner(newOwner:Symbol)(using Context) =
      copy(nested = nested.changeOwner(newOwner))  
         

    private def prefixTerm(nestedTerm: Tree, isPure: Boolean)(using Context, CpsTopLevelContext): Tree =
      records.foldLeft(nestedTerm){(s,e) => 
        e.prefixTerm(s, isPure)
      }

    override def show(using Context): String = {
      val sRecords = records.map(_.show).mkString(".")
      s"SelectTypeApplyCpsTree(${nested.show},${sRecords})"
    }  
  

}

object SelectTypeApplyTypedCpsTree {

   sealed trait Operation {
      def prefixTerm(term:Tree, isPure: Boolean)(using Context, CpsTopLevelContext): Tree
      def show(using Context): String
   }

   case class OpSelect(origin:Select) extends Operation {

      override def prefixTerm(term:Tree, isPure: Boolean)(using Context, CpsTopLevelContext): Tree =
        val sym = origin.symbol
        val ntpe = TermRef(term.tpe, origin.name.toTermName, sym.denot.asSeenFrom(term.tpe))
        Select(term,Types.TermRef(term.tpe,sym)).withType(ntpe).withSpan(origin.span)

      override def show(using Context) = {
        origin.symbol.name.toString
      }
   }

   case class OpTypeApply(origin:TypeApply) extends Operation {

      override def prefixTerm(term:Tree, isPure: Boolean)(using Context, CpsTopLevelContext): Tree = {
        if (origin.args.isEmpty) then
          term
        else
          TypeApply(term,origin.args).withSpan(origin.span)
      }

      override def show(using Context) = {
        val targs = origin.args.map(_.show).mkString(",")
        s"[$targs]"
      }

   }

   case class OpTyped(origin:Typed) extends Operation {

      override def prefixTerm(term:Tree, isPure: Boolean)(using Context, CpsTopLevelContext): Tree = {
        if (isPure) then
          Typed(term,origin.tpt).withSpan(origin.span)
        else
          val nTpt = CpsTransformHelper.cpsTransformedType(origin.tpt.tpe, summon[CpsTopLevelContext].monadType)
          Typed(term,TypeTree(nTpt)).withSpan(origin.span)
      }

      override def show(using Context) = {
        s"typedCps[${origin.tpt.show}]"
      }

   }


}
