package cps

trait AsyncShift[T]

object AsyncShift {

 inline given shiftedArrayOps[A] as _ <: AsyncShift[scala.collection.ArrayOps[A]] = 
      new cps.runtime.ArrayOpsAsyncShift[A]()

}

