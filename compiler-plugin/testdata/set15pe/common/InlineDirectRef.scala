package cps.pe

import cps.{*,given}
import scala.annotation.experimental

@experimental
object inlineDirectRef {

  opaque type InlineDirectRef[T<:AnyRef] = PERef[T]

  opaque type InlineDirectIntRef = PEIntRef

  object InlineDirectRef {

    def apply[T<:AnyRef](ref:PERef[T]): InlineDirectRef[T] =
      ref

  }

  extension [T <: AnyRef](ref: InlineDirectRef[T]) {

    inline def get(using CpsDirect[PureEffect]): T =
      await(ref.get())

    inline def set(t:T)(using CpsDirect[PureEffect]): Unit =
      await(ref.set(t))

    inline def update(f:T=>T)(using CpsDirect[PureEffect]): Unit =
      await(ref.update(f))

    inline def updateAndGet(f:T=>T)(using CpsDirect[PureEffect]): T =
      await(ref.updateAndGet(f))

    inline def getAndSet(t:T)(using CpsDirect[PureEffect]): T =
      await(ref.getAndSet(t))

    inline def compareAndSet(except:T, v:T)(using CpsDirect[PureEffect]): Boolean =
      await(ref.compareAndSet(except,v))

  }

  object InlineDirectIntRef {

    def apply(ref:PEIntRef): InlineDirectIntRef =
      ref

  }

  extension (ref: InlineDirectIntRef) {

    inline def get()(using CpsDirect[PureEffect]): Int =
      await(ref.asInstanceOf[PEIntRef].get())

    inline def set(t:Int)(using CpsDirect[PureEffect]): Unit =
      await(ref.set(t))

    inline def update(f:Int=>Int)(using CpsDirect[PureEffect]): Unit =
      await(ref.update(f))

    inline def updateAndGet(f:Int=>Int)(using CpsDirect[PureEffect]): Int =
      await(ref.updateAndGet(f))

    inline def getAndSet(t:Int)(using CpsDirect[PureEffect]): Int = {
      await(ref.asInstanceOf[PEIntRef].getAndSet(t))
    }

    inline def compareAndSet(except:Int, v:Int)(using CpsDirect[PureEffect]): Boolean = {
      await(ref.asInstanceOf[PEIntRef].compareAndSet(except,v))
    }

    inline def increment()(using CpsDirect[PureEffect]): Int =
      await(ref.increment())

    inline def decrement()(using CpsDirect[PureEffect]): Int =
      await(ref.decrement())

  }

  extension (p:PureEffect.type) {

    def inlineDirectRef[T<:AnyRef](t:T)(using CpsDirect[PureEffect]): InlineDirectRef[T] =
      InlineDirectRef[T](await(PERef.make(t)))

    def inlineDirectIntRef(t:Int)(using CpsDirect[PureEffect]): InlineDirectIntRef =
      InlineDirectIntRef(await(PEIntRef.make(t)))

  }

}
