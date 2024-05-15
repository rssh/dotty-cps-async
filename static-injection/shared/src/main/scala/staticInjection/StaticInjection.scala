package staticInjection

import scala.annotation.*
import scala.compiletime.*

trait StaticInjectionContext

transparent inline def injectfull[A](inline body: StaticInjectionContext ?=> A): Any = {
   error("Implement me")
}


@compileTimeOnly("Inject should-be eliminated by injectfull")
def inject[A](using StaticInjectionContext):A = {
  ???
}
