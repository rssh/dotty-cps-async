package cps.customValueDiscard

import cps._

trait Tag

/**
 * marker object for value discarding.
 *  When this object is imported into current scope,
 *  then discarding values inside async block is translated
 *  to summon[ValueDiscard[T]].apply()
 **/
given tag: Tag with {}




