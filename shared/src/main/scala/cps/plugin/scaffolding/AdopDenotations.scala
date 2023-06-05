package cps.plugin.scaffolding

import scala.annotation.compileTimeOnly

/**
 * This function are formal functions, which adopt transformed body of async function to
 *  old shape, which can't be changed in compiler plugin before erase step.
 */

//TODO: submit issue to dotty, that we need to have some way to remove function after erasure.
//       (move checking compileTimeOnly after erasure)
//
//
// cpsed function body is wrapped into adoptForUncopsedDenotation to 
//
// we can't use compileTimeOnly for this function, because compileTimeOnly is checked before erasure, and we need to remove it after erasure
//  when we can change symbol dentations.
//@compileTimeOnly("adoptForUncpsedDenotation should be removed by dotty-cps-async compiler plugin after symbol dentations will changed")
def adoptForUncpsedDenotation[F[_],T](f: F[T]): T = {
  println("adoptForUncpsedDenotation should be removed by dotty-cps-async compiler plugin after symbol dentations will changed")
  println("looks like you cathed bug in plugin, please report it")
  ???
}

//
//@compileTimeOnly
def adoptFunForUncpsedDenotation[F[_],A,B](f: A=>B): A=>F[B] = {
  ???
}


//@compileTimeOnly("adoptCpsedCall should be removed on compiler plugin after symbol dentations will changed")
def adoptCpsedCall[F[_],T](f: T): F[T] = {
  println("adoptCpsedCall should be removed by dotty-cps-async compiler plugin after symbol dentations will changed")
  println("looks like you cathed bug in plugin, please report it")
  ???
}
