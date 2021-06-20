package cps.automaticColoring

trait AutomaticColoringTag[F[_]]
 
/**
 * import of this tag enable automatic  coloring 
 **/
given tag[F[_]]: AutomaticColoringTag[F] with {}


/**
 * if this tag is implemented for some monad, then non-unit value discards become warnings in async block, 
 *  (otherwise they errors).
 **/
trait WarnValueDiscard[F[_]]

