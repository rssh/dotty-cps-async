package cps.macros.observatory

import scala.collection.*
import scala.quoted.*

class PerSymbolStorage[T]:

   // we know, that symbol-id is hashcode.
   //   this is not reflected in public-id.
   private val storage: mutable.Map[Int,T] = mutable.Map()

   def get(using Quotes)(symbol: quotes.reflect.Symbol): Option[T] =
    storage.get(symbol.hashCode)

   def getOrUpdate(using Quotes)(symbol: quotes.reflect.Symbol, defaultValue: =>T): T =
    storage.get(symbol.hashCode) match
       case Some(v) => v
       case None => val newRecord = defaultValue
              storage.update(symbol.hashCode, newRecord)
              newRecord

   def update(using Quotes)(symbol: quotes.reflect.Symbol, value: T): Unit =
    storage.update(symbol.hashCode, value)

   def remove(using Quotes)(symbol: quotes.reflect.Symbol): Option[T] =
    storage.remove(symbol.hashCode)

   def foreach(f: T=>Unit): Unit =
    storage.foreach((k,v) => f(v))

