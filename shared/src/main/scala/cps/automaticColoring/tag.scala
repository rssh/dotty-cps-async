package cps.automaticColoring

trait AutomaticColoringTag[F[_]]

 
/**
 * if this tag is set for some monad, then non-unit value discards become warnings in async block, 
 *  (otherwise they errors).
 **/
@Deprecated("use -Wvalue-discard instead")
trait WarnValueDiscard[F[_]]

/**
* import of this tag enable automatic  coloring 
**/
@Deprecated("use direct context functions instead")
given tag[F[_]]: AutomaticColoringTag[F] with {}


