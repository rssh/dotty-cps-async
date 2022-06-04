package futureScope.util


case class NoEscalateExceptionWrapper(e:Throwable) extends RuntimeException("NonEscalateExceptionWrapper",e)
