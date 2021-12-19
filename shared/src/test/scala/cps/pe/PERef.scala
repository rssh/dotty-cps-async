package cps.pe

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicInteger

class PERef[T<:AnyRef](initValue: T):

  val value = new AtomicReference[T](initValue)

  def get(): PureEffect[T] =
    PureEffect.delay(value.get().nn)

  def update(f: T=>T): PureEffect[Unit] = 
    PureEffect.delay{  value.updateAndGet(x => f(x.nn)); }

  def updateAndGet(f: T=>T): PureEffect[T] = 
    PureEffect.delay( value.updateAndGet(x => f(x.nn)).nn )

  def set(v: T): PureEffect[Unit] =
    PureEffect.delay( value.set(v) )

  def getAndSet(v: T): PureEffect[T] =
    PureEffect.delay( value.getAndSet(v).nn )
   
  def compareAndSet(except: T, v: T): PureEffect[Boolean] =
    PureEffect.delay( value.compareAndSet(except, v) )

object PERef:

  def make[T <: AnyRef](value: =>T):PureEffect[PERef[T]] =
     PureEffect.delay( new PERef[T](value) )


class PEIntRef(initValue: Int):

  val value = new AtomicInteger(initValue)

  def get(): PureEffect[Int] =
    PureEffect.delay(value.get())

  def set(v:Int): PureEffect[Unit] =
    PureEffect.delay({value.set(v); ()})

  def getAndSet(v:Int): PureEffect[Int] =
    PureEffect.delay(value.getAndSet(v))

  def update(f: Int=>Int): PureEffect[Unit] = 
    PureEffect.delay({ value.updateAndGet(x=>f(x)); () })


  def increment(): PureEffect[Int] =
    PureEffect.delay(value.incrementAndGet())

  def decrement(): PureEffect[Int] =
    PureEffect.delay(value.decrementAndGet())

  // for checking in test
  def __get(): Int =
     value.get()

object PEIntRef:

  def make(value:Int = 0):PureEffect[PEIntRef] =
    PureEffect.delay(new PEIntRef(value))


