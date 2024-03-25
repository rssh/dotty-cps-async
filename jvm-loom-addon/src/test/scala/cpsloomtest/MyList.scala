package cpsloomtest


sealed trait MyList[+A] {

  def map[B](f: A=>B): MyList[B]

  def length: Int

}

case object MyNil extends MyList[Nothing] {

  override def map[B](f: Nothing => B): MyList[B] = MyNil

  override def length: Int = 0

}

case class MyCons[T](hd: T, tl: MyList[T]) extends MyList[T] {

  override def map[B](f: T => B): MyList[B] = MyCons(f(hd), tl.map(f))

  override def length: Int = 1 + tl.length

}

object MyList {

  def create[T](args: T*):MyList[T] = {
    if (args.isEmpty) MyNil
    else MyCons(args.head, create(args.tail*))
  }

}


