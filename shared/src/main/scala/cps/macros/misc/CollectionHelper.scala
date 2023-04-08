package cps.macros.misc


import scala.quoted.*
import scala.collection.IterableFactory

object CollectionHelper {


    inline def retrieveIterableFactory[CC[x]<:Iterable[x]]: IterableFactory[CC] = ${
      retrieveIterableFactoryImpl[CC]
    } 

    def retrieveIterableFactoryImpl[CC[x]<:Iterable[x]:Type](using Quotes): Expr[IterableFactory[CC]] = {
      import quotes.reflect.*
      val ccTpe = TypeRepr.of[CC]
      val companion = Ref(ccTpe.typeSymbol.companionModule)
      if (companion.tpe <:< TypeRepr.of[IterableFactory[CC]]) then
        companion.asExprOf[IterableFactory[CC]]
      else
        report.errorAndAbort(s"companion for ${ccTpe.show} is not an IterableFactory")
    }

}

