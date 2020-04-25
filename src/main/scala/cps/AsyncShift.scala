package cps
 
import scala.collection.Seq

trait AsyncShift[+T]

object AsyncShift {

 transparent inline given shiftedArrayOps[A] as AsyncShift[scala.collection.ArrayOps[A]] = 
      new cps.runtime.ArrayOpsAsyncShift[A]()

 transparent inline given shiftedSeq[A] as AsyncShift[Seq[A]] =
      new cps.runtime.SeqAsyncShift[A]()

 transparent inline given shiftedList[A] as AsyncShift[List[A]] =
      new cps.runtime.ListAsyncShift[A]()

 transparent inline given shiftedSet[A] as AsyncShift[Set[A]] =
      new cps.runtime.ImmutableSetAsyncShift[A]()


}

