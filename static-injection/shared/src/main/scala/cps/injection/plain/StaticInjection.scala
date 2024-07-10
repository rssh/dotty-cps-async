package cps.injection.plain

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.quoted.*

object CompileTimeConstants {

  val inject = "cps.injection.plain.inject"

}

transparent inline def injectfull[A](inline body: A): Any = ${
  inferInjects[A]('body)
}


def inferInjects[A: Type](bodyExpr: Expr[A])(using quotes: Quotes): Expr[Any] = {
  import quotes.reflect.{*, given}

  val types = new ArrayBuffer[TypeRepr]()

  val injectInContext = new TreeMap() {
    override def transformTerm(term: Term)(owner: Symbol): Term =
      term match
        case TypeApply(fun, List(injectType)) if fun.symbol == Symbol.requiredMethod(CompileTimeConstants.inject) =>
          Implicits.search(injectType.tpe) match
            case iss: ImplicitSearchSuccess =>
              iss.tree
            case isf: ImplicitSearchFailure =>
              types += injectType.tpe
              super.transformTerm(term)(owner)
        case t =>
          super.transformTerm(term)(owner)
  }.transformTerm(bodyExpr.asTerm)(Symbol.spliceOwner)
  
  val injectAll = types match {
    case types if types.sizeIs > 0 =>
      Lambda(
        Symbol.spliceOwner,
        MethodType(MethodTypeKind.Implicit)(types.indices.map(num => s"t$num").toList)(_ => types.toList, _ => TypeRepr.of[A]),
        (owner, params) =>
          new TreeMap() {
            var paramsCounter = 0
            override def transformTerm(term: Term)(owner: Symbol): Term =
              term match
                case TypeApply(fun, List(injectType)) if fun.symbol == Symbol.requiredMethod(CompileTimeConstants.inject) =>
                  val termRef = Ref(params(paramsCounter).symbol)
                  paramsCounter += 1
                  termRef
                case t =>
                  super.transformTerm(term)(owner)
          }.transformTerm(injectInContext)(Symbol.spliceOwner)
            .changeOwner(owner)
      )
    case _ => injectInContext
  }
  
  injectAll.asExpr
}

@compileTimeOnly("Inject should-be eliminated by injectfull")
def inject[A]: A = {
  ???
}