package cps.automaticColoring

trait AutomaticColoringTag[F[_]]

 
/**
 * if this tag is set for some monad, then non-unit value discards become warnings in async block, 
 *  (otherwise they errors).
 **/
trait WarnValueDiscard[F[_]]

/**
* import of this tag enable automatic  coloring 
**/
given tag[F[_]]: AutomaticColoringTag[F] with {}


