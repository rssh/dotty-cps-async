package cps.gopherlike

import cps._

class SLSelectLoop[F[_]:CpsMonad]:

  case class ReadRecord[A](Reader: IFReader[F,A], handler: A=>F[Boolean])

  var readers: List[ReadRecord[?]] = List.empty

  def onReadAsync[A](ch:IFReader[F,A])(f: A=>F[Boolean]): this.type =
      readers = ReadRecord(ch,f)::readers
      this

