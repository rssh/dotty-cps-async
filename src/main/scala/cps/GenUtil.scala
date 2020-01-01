package cps

import scala.compiletime._
import scala.quoted._

object GenUtil {

  class TypeHolder[T](tp:Type[T]) {
    type V = T
  }

  inline def defaultValue[T:Type](given qctx: QuoteContext) = {
    inline erasedValue[T] match {
      case _: Byte => (0: Byte)
      case _: Char => (0: Char)
      case _: Short => (0: Short)
      case _: Int => (0)
      case _: Long => (0L)
      case _: Float => (0.0f)
      case _: Double => (0.0d)
      case _: Boolean => (false)
      case _: Unit => ()
      case _: AnyRef => null
      case _ => 
              val msg = s"Can't find default value for ${summon[Type[T]]}"
              qctx.error(msg)
    }
  }


}
