package cps.automaticColoring

trait AutomaticColoringTag
 
/**
 * import of this tag enable automatic  coloring 
 **/
given tag: AutomaticColoringTag with {}


/**
 * if this tag is implemented for some monad, then non-unit value discards become warnings in async block, 
 *  (otherwise they errors).
 **/
trait WarnValueDiscard[F[_]]

