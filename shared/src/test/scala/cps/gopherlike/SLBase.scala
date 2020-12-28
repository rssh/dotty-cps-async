package cps.gopherlike

import cps._

class SLSelectLoop[F[_]:CpsMonad]:

  case class ReadRecord[A](reader: IFReader[F,A], handler: A=>F[Boolean])

  var readers: List[ReadRecord[?]] = List.empty

  def onReadAsync[A](ch:IFReader[F,A])(f: A=>F[Boolean]): this.type =
      readers = ReadRecord(ch,f)::readers
      this


  def runAsync():F[Unit] =
    if (readers.isEmpty)
      summon[CpsMonad[F]].pure(())
    else
      summon[CpsMonad[F]].map(readers.head.reader.aread())(a => ())
      
