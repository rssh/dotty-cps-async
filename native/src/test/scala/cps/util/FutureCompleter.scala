package cps.util

import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

// for use futures in jvm and native test.
object FutureCompleter
{

  def apply[T](f: Future[T])(using ec:ExecutionContext): Future[Try[T]] = 
    f.transformWith( r => Future successful r )

}
