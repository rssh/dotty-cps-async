package cps.plugin


enum AsyncKind  {

  case Sync extends AsyncKind
  case Async(internalKind:AsyncKind) extends AsyncKind
  case AsyncLambda(bodyKind: AsyncKind) extends AsyncKind

  

}

