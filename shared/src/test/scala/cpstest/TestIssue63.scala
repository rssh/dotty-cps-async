package cpstest


import cps.*
import cps.monads.{*, given}


import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cps.testconfig.given

// if `A` is replaced with concrete type it compiles
def futureFetchList[A](l: List[Future[A]]): Future[List[A]] =
  async[Future] {
    //l.map(await(_))
    l.map[A](x => await(x))
  }


