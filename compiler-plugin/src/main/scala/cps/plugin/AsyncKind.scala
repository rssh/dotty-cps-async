package cps.plugin

import cps.plugin.AsyncKind.AsyncLambda


enum AsyncKind  {

  case Sync extends AsyncKind
  case Async(internalKind:AsyncKind) extends AsyncKind
  case AsyncLambda(bodyKind: AsyncKind) extends AsyncKind


  def unify(other: AsyncKind): Either[(AsyncKind,AsyncKind),AsyncKind] =
    (this,other) match
      case (Sync, Sync) => Right(Sync)
      case (Async(a), Async(b)) => a.unify(b).map(Async(_))
      case (AsyncLambda(a), AsyncLambda(b)) => a.unify(b).map(AsyncLambda(_))
      case (Async(a), Sync) => a.unify(Sync).map(Async(_))
      case (Sync, Async(a)) => a.unify(Sync).map(Async(_))
      case _ => Left((this,other))

  /**
   * True, if those kinds can be in sibling branches of `ìf` or `match`.
   * @param other
   * @return
   */
  def isCompatible(other: AsyncKind): Boolean =
    (this,other) match
      case (Sync, Sync) => true
      case (Async(a), Sync) => a.isCompatible(AsyncKind.Sync)
      case (AsyncLambda(a), Sync) =>
                 // here in theory we can check async-tree and try transform righ-part of async-lambda.
                 // Leave it for future.
                 false
      case (Sync, Async(b)) => b.isCompatible(AsyncKind.Sync)
      case (Sync, AsyncLambda(b)) => false
      case (Async(a), Async(b)) => a.isCompatible(b)
      case (AsyncLambda(a), Async(b)) => false
      case (Async(a), AsyncLambda(b)) => false
      case (AsyncLambda(a), AsyncLambda(b)) => a.isCompatible(b)

  def isSync: Boolean =
    this match
      case Sync => true
      case Async(_) => false
      case AsyncLambda(_) => false

  def isAsync: Boolean =
    this match
      case Sync => false
      case Async(_) => true
      case AsyncLambda(_) => false

   def isAsyncLambda: Boolean =
     this match
        case Sync => false
        case Async(_) => false
        case AsyncLambda(_) => true

}

