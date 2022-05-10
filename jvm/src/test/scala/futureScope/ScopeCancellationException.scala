package futureScope

class ScopeCancellationException(message:String = "cancel", ex: Throwable|Null = null) extends RuntimeException(message) {

   if ex != null then
      initCause(ex)

}

object ScopeCancellationException {

   def unapply(v:ScopeCancellationException):Option[(String,Throwable|Null)] =
     Some(v.getMessage.nn, v.getCause)


}

class ScopeFinished extends ScopeCancellationException("scopeFinished")

object ScopeFinished {

   def unapply(x:ScopeFinished): Boolean =
    true

}

