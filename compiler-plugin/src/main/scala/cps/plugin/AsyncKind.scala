package cps.plugin


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

}

