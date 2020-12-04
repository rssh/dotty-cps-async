package cps.features

import cps._

// bug in dotty:
trait customValueDiscard
trait warningValueDiscard

trait CustomValueDiscardTag
trait WarnValueDiscardTag

/**
 * marker object for value discarding.
 *  When this object is imported into current scope,
 *  then discarding values inside async block is translated
 *  to summon[ValueDiscard[T]].apply()
 **/
object customValueDiscard:

  given tag: CustomValueDiscardTag with {}



/**
 * marker object for warning about value discarding.
 **/
object warnValueDiscard:

  given tag: WarnValueDiscardTag with {}




