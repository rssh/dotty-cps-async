package cps.pe

import cps.{*,given}
import scala.annotation.experimental

@experimental
object directRef {

  opaque type DirectRef[T<:AnyRef] = PERef[T]

  opaque type DirectIntRef = PEIntRef

  object DirectRef {

      def apply[T<:AnyRef](ref:PERef[T]): DirectRef[T] =
        ref

  }

  extension [T <: AnyRef](ref: DirectRef[T]) {

    def get(using CpsDirect[PureEffect]): T =
      await(ref.get())

    def set(t:T)(using CpsDirect[PureEffect]): Unit =
      await(ref.set(t))

    def update(f:T=>T)(using CpsDirect[PureEffect]): Unit =
      await(ref.update(f))

    def updateAndGet(f:T=>T)(using CpsDirect[PureEffect]): T =
      await(ref.updateAndGet(f))

    def getAndSet(t:T)(using CpsDirect[PureEffect]): T =
      await(ref.getAndSet(t))

    def compareAndSet(except:T, v:T)(using CpsDirect[PureEffect]): Boolean =
      await(ref.compareAndSet(except,v))

  }
  

  extension (self: DirectRef.type)  {

      def make[T<:AnyRef](t:T)(using CpsDirect[PureEffect]): DirectRef[T] =
          DirectRef[T](await(PERef.make(t)))

  }

  object DirectIntRef {

      def apply(ref:PEIntRef): DirectIntRef =
        ref

  }

  extension (ref: DirectIntRef) {

    def get()(using CpsDirect[PureEffect]): Int =
      await(ref.asInstanceOf[PEIntRef].get())

    def set(t:Int)(using CpsDirect[PureEffect]): Unit =
      await(ref.set(t))

    def update(f:Int=>Int)(using CpsDirect[PureEffect]): Unit =
      await(ref.update(f))

    def updateAndGet(f:Int=>Int)(using CpsDirect[PureEffect]): Int =
      await(ref.updateAndGet(f))

    def getAndSet(t:Int)(using CpsDirect[PureEffect]): Int = {
      await(ref.asInstanceOf[PEIntRef].getAndSet(t))
    }

    def compareAndSet(except:Int, v:Int)(using CpsDirect[PureEffect]): Boolean = {
      await(ref.asInstanceOf[PEIntRef].compareAndSet(except,v))
    }

    def increment()(using CpsDirect[PureEffect]): Int =
      await(ref.increment())

    def decrement()(using CpsDirect[PureEffect]): Int =
      await(ref.decrement())

  }

  extension (p:PureEffect.type) {

    def directRef[T<:AnyRef](t:T)(using CpsDirect[PureEffect]): DirectRef[T] =
      DirectRef[T](await(PERef.make(t)))

    def directIntRef(t:Int)(using CpsDirect[PureEffect]): DirectIntRef =
      DirectIntRef(await(PEIntRef.make(t)))

  }

}