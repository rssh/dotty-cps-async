package cps.stream

import cps.*
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.*
import scala.util.*
import scala.collection.Factory
import scala.collection.mutable.Growable


/**
 * Minimal mutable async stream.
 **/
trait AsyncIterator[F[_]:CpsConcurrentMonad, T]:

  thisAsyncIterator =>

  /**
  * return the next element of stream in option or None if stream is finished.
  **/
  def  next: F[Option[T]]


  /**
   * return iterator with values mapped by `f`
   **/
  def  map[S](f: T=>S): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    def next: F[Option[S]] = 
      summon[CpsConcurrentMonad[F]].map(thisAsyncIterator.next)(_.map(f))
  }

  /**
   * map over async function. Substituted automatically when using await 
   * inside async block in map.
   **/
  def  mapAsync[S](f: T=> F[S]): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    def next: F[Option[S]] = 
      summon[CpsConcurrentMonad[F]].flatMap(thisAsyncIterator.next){ ov =>
        ov match
          case Some(v) => summon[CpsConcurrentMonad[F]].map(f(v))(Some(_))
          case None => summon[CpsConcurrentMonad[F]].pure(None)
      }
  }


  /**
   * map over Try[T], which allows to handle the cases, when `next` returns a failure.
   * Be carefuel for situation, when failed next have no effect - in this case we will
   * receive the infinite sequence of failuers.
   **/
  def mapTry[S](f: Try[T]=>S): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    def next: F[Option[S]] = 
      summon[CpsConcurrentMonad[F]].mapTry(thisAsyncIterator.next){ tx => 
        tx match {
          case Success(Some(x)) => 
             Some(f(Success(x))) 
          case Success(None) =>
             None
          case Failure(ex) =>
             Some(f(Failure(ex)))  
        }
      }
  }

  /**
   * async version of mapTry
   **/
  def mapTryAsync[S](f: Try[T] => F[S]): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    def next:F[Option[S]] =
      val m = summon[CpsConcurrentMonad[F]]
      m.flatMapTry(thisAsyncIterator.next){ tx =>
        tx match
          case Success(Some(x)) => 
            m.map(f(Success(x)))(Some(_))
          case Success(None) =>
            m.pure(None)
          case Failure(ex) =>
            m.map(f(Failure(ex)))(Some(_))
      }
  }

  /**
   * synonym for `mapTry(identity)`
   **/
  def inTry: AsyncIterator[F,Try[T]] = 
    this.mapTry(identity)

  
  def flatMap[S](f: T => AsyncIterator[F,S]): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    val refInternal = new AtomicReference[AsyncIterator[F,S]](AsyncIterator.empty)
    override def next: F[Option[S]] =
      summon[CpsMonad[F]].flatMap(refInternal.get.nn.next){ vi =>
        vi match
          case r@Some(s) => summon[CpsMonad[F]].pure(r)
          case None =>
            summon[CpsMonad[F]].flatMap(thisAsyncIterator.next){ ti =>
              ti match
                case Some(t) => 
                  refInternal.set(f(t))
                  next
                case None => 
                  summon[CpsMonad[F]].pure(None)
            }

      }
  }

  def flatMapAsync[S](f: T => F[AsyncIterator[F,S]]): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    val refInternal = new AtomicReference[AsyncIterator[F,S]](AsyncIterator.empty)
    override def next: F[Option[S]] =
      summon[CpsMonad[F]].flatMap(refInternal.get.nn.next){ si =>
        si match
          case r@Some(s) => summon[CpsMonad[F]].pure(r)
          case None =>
            summon[CpsMonad[F]].flatMap(thisAsyncIterator.next){ ti =>
              ti match
                case Some(t) =>
                  summon[CpsMonad[F]].flatMap(f(t)){ ft =>
                    refInternal.set(ft)
                    next
                  }
                case None => 
                  summon[CpsMonad[F]].pure(None)
            }
      }
  }

  def flatMapTry[S](f: Try[T] => AsyncIterator[F,S]): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    val refInternal = new AtomicReference[AsyncIterator[F,S]](AsyncIterator.empty)
    override def next: F[Option[S]] = {
      summon[CpsMonad[F]].flatMap(refInternal.get.nn.next){ si =>
         si match
            case r@Some(s) => summon[CpsMonad[F]].pure(r)
            case None =>
              summon[CpsMonad[F]].flatMapTry(thisAsyncIterator.next){ 
                  case Success(Some(t)) =>
                    refInternal.set(f(Success(t)))
                    next
                  case Success(None) => 
                    summon[CpsMonad[F]].pure(None)
                  case Failure(ex) =>
                    refInternal.set(f(Failure(ex)))
                    next
              }
      }
    }
  }

  def flatMapTryAsync[S](f: Try[T] => F[AsyncIterator[F,S]]): AsyncIterator[F,S] = new AsyncIterator[F,S] {
    val refInternal = new AtomicReference[AsyncIterator[F,S]](AsyncIterator.empty)
    override def next: F[Option[S]] = {
      summon[CpsMonad[F]].flatMap(refInternal.get.nn.next){ si =>
         si match
            case r@Some(s) => summon[CpsMonad[F]].pure(r)
            case None =>
              summon[CpsMonad[F]].flatMapTry(thisAsyncIterator.next){ 
                  case Success(Some(t)) =>
                    summon[CpsMonad[F]].flatMap(f(Success(t))){ ft =>
                      refInternal.set(ft)
                      next
                    }
                  case Success(None) => 
                    summon[CpsMonad[F]].pure(None)
                  case Failure(ex) =>
                    summon[CpsMonad[F]].flatMap(f(Failure(ex))){ ft =>
                      refInternal.set(ft)
                      next
                    }
              }
      }
    }
  }


  /**
   * filter accumulator by p, returning only those values, which as satisficy `p`.
   **/
  def filter(p: T=>Boolean): AsyncIterator[F,T] = new AsyncIterator[F, T] {
    def next: F[Option[T]] =
      summon[CpsConcurrentMonad[F]].flatMap(thisAsyncIterator.next){ ov =>
        ov match
          case Some(v) => if (p(v)) summon[CpsConcurrentMonad[F]].pure(Some(v)) else next
          case None => summon[CpsConcurrentMonad[F]].pure(None)
      }
  }

  /**
   * filter accumulator by p, returning only those values, which as satisficy `p`.
   * Note, that `p` is applied sequentially
   **/
  def filterAsync(p: T=>F[Boolean]): AsyncIterator[F,T] = new AsyncIterator[F, T] {
    def next: F[Option[T]] =
      summon[CpsConcurrentMonad[F]].flatMap(thisAsyncIterator.next){ ov =>
        ov match
          case Some(v) => summon[CpsConcurrentMonad[F]].flatMap(p(v)){ c =>
                if (c) then 
                  summon[CpsConcurrentMonad[F]].pure(Some(v))
                else
                  next
            }
          case None => summon[CpsConcurrentMonad[F]].pure(None)
      }
  }

  /**
   * Find first value wich satisficy `p`
   **/
  def find(p: T=>Boolean): F[Option[T]] = {
    summon[CpsConcurrentMonad[F]].flatMap(next){
       case Some(v) => 
          if p(v) then  summon[CpsConcurrentMonad[F]].pure(Some(v)) else find(p)
       case None => summon[CpsConcurrentMonad[F]].pure(None)
    }
  }

  def findAsync(p: T=>F[Boolean]): F[Option[T]] =
    summon[CpsConcurrentMonad[F]].flatMap(next) {
      case Some(v) =>
        summon[CpsConcurrentMonad[F]].flatMap(p(v)){ c =>
          if (c) summon[CpsConcurrentMonad[F]].pure(Some(v)) else findAsync(p)
        }
      case None => summon[CpsConcurrentMonad[F]].pure(None)
    }


  def fold[S](s0:S)(f:(S,T)=>S): F[S] = 
    summon[CpsConcurrentMonad[F]].flatMap(next){ 
      case Some(v) => fold(f(s0,v))(f)
      case None => summon[CpsConcurrentMonad[F]].pure(s0)
    }
  

  def foldAsync[S](s0:S)(f:(S,T)=>F[S]): F[S] =
    summon[CpsConcurrentMonad[F]].flatMap(next){
      case Some(v) =>
        summon[CpsConcurrentMonad[F]].flatMap(f(s0,v)){ s => foldAsync(s)(f) }
      case None => summon[CpsConcurrentMonad[F]].pure(s0)  
    }

  /**
   * Scan the value and output in the resulting iterator cummulative accumulated values.  
   * Note, that
   * - `f` should be side effects free, since it can be reapplied in
   *  situation, when  parallel threads tryng to read the next value
   * - s0 and f(s,t) should not be nulls.
   **/  
  def scan[S](s0:S)(f:(S,T)=>S): AsyncIterator[F,S] = new AsyncIterator[F,S]{
      val sRef: AtomicReference[S|Null] = new AtomicReference(null)

      def advance(t:T): S =
        var sNext: S|Null = null
        while {
           val s = sRef.get.nn
           sNext = f(s,t)
           !sRef.compareAndSet(s,sNext)
        } do()
        sNext.nn

      def next: F[Option[S]] = {
        if (sRef.compareAndSet(null,s0)) {
          summon[CpsConcurrentMonad[F]].pure(Some(s0))
        } else {
          summon[CpsConcurrentMonad[F]].map(thisAsyncIterator.next)( ot =>
            ot.map(t => advance(t))
            //scaaljs have no updateAndGet
            // TODO: update scalajs
            //ot.map(t=> sRef.updateAndGet( s => f(s.nn,t) ).nn )
          )
        }
      }
  }

  /**
   * Scan the value and output in the resulting iterator cummulative accumulated values.  
   * Note, that
   * - `f` should be side effects free, since it can be reapplied in
   *  situation, when  parallel threads tryng to read the next value
   * - s0 and f(s,t) should not be nulls.
   **/  
  def scanAsync[S](s0:S)(f:(S,T)=>F[S]): AsyncIterator[F,S] = new AsyncIterator[F,S]{
      val sRef: AtomicReference[S|Null] = new AtomicReference(null)

      def advance(sPrev:S, t:T):F[Option[S]] =
        summon[CpsConcurrentMonad[F]].flatMap(f(sPrev,t)){ r =>
          if (sRef.compareAndSet(sPrev,r)) {
            summon[CpsConcurrentMonad[F]].pure(Some(r))
          } else {
            val sNow = sRef.get.nn
            advance(sNow,t)
          }
        }

      def next: F[Option[S]] = {
        if (sRef.compareAndSet(null,s0)) {
          summon[CpsMonad[F]].pure(Some(s0))
        } else {
          summon[CpsMonad[F]].flatMap(thisAsyncIterator.next){ 
             case Some(t) =>
               val sPrev = sRef.get.nn
               advance(sPrev,t)
             case None =>
               summon[CpsMonad[F]].pure(None)
          }
        }
      }
  }

  def takeTo[B <: Growable[T]](buffer: B, n: Int):F[B] = {
    if (n != 0) then
      summon[CpsMonad[F]].flatMap(next){
        case Some(v) => buffer.addOne(v)
                        takeTo(buffer, n-1)
        case None => summon[CpsMonad[F]].pure(buffer)
      }
    else 
      summon[CpsMonad[F]].pure(buffer)  
  }

  def takeVector(n: Int): F[Vector[T]] = {
    import scala.collection.immutable.VectorBuilder
    val vb = VectorBuilder[T]()
    summon[CpsMonad[F]].map(takeTo(vb, n))(_.result)
  }
  
  def takeList(n: Int): F[List[T]] = {
    import scala.collection.mutable.ListBuffer
    val lb = ListBuffer[T]()
    summon[CpsMonad[F]].map(takeTo(lb,n))(_.result)
  }

  def take[CC[_]](n:Int)(using Factory[T, CC[T]]):F[CC[T]] = {
     val builder = summon[Factory[T,CC[T]]].newBuilder
     summon[CpsMonad[F]].map(takeTo(builder,n))(_.result)
  }

  def takeAll[CC[_]](n:Int)(using Factory[T,CC[T]]):F[CC[T]] =
     take[CC](-1)

  

end AsyncIterator

    

object AsyncIterator:

   def unfold[S,F[_]:CpsConcurrentMonad,T](s0:S)(f:S => F[Option[(T,S)]])(using ExecutionContext): AsyncIterator[F,T] =
     AsyncListIterator(AsyncList.unfold(s0)(f))

   given absorber[F[_],C<:CpsMonadContext[F],T](using ExecutionContext, CpsConcurrentMonad.Aux[F,C]): CpsAsyncEmitAbsorber4[AsyncIterator[F,T],F,C,T] =
     AsyncIteratorEmitAbsorber[F,C,T]()

   def empty[F[_]:CpsConcurrentMonad,T] = new AsyncIterator[F,T] {
     override def next: F[Option[T]] = summon[CpsMonad[F]].pure(None)
   }

   def error[F[_]:CpsConcurrentMonad,T](e: Throwable) = new AsyncIterator[F,T] {
     override def next: F[Option[T]] = summon[CpsTryMonad[F]].error(e)
   }

   def one[F[_]:CpsConcurrentMonad,T](value:T) = new AsyncIterator[F,T] {
     val readFlag = new AtomicBoolean(false)
     override def next: F[Option[T]] = {
       val v = {
          if readFlag.compareAndSet(false,true) then
            Some(value)
          else
            None
       }
       summon[CpsMonad[F]].pure(v)
     }
   }

   given [F[_]:CpsConcurrentMonad]: CpsTryMonad[[T] =>> AsyncIterator[F,T]] with CpsTryMonadInstanceContext[[T]=>>AsyncIterator[F,T]] with
     override def pure[A](a:A):AsyncIterator[F,A] = one(a)
     override def map[A,B](fa:AsyncIterator[F,A])(f:A=>B):AsyncIterator[F,B] = fa.map(f)
     override def flatMap[A,B](fa:AsyncIterator[F,A])(f:A=>AsyncIterator[F,B]):AsyncIterator[F,B] = fa.flatMap(f)
     override def error[A](e:Throwable):AsyncIterator[F,A] = AsyncIterator.error(e)
     override def mapTry[A,B](fa:AsyncIterator[F,A])(f: Try[A]=>B):AsyncIterator[F,B] = fa.mapTry(f)
     override def flatMapTry[A,B](fa:AsyncIterator[F,A])(f: Try[A]=>AsyncIterator[F,B]):AsyncIterator[F,B] = fa.flatMapTry(f)
     
     

class AsyncIteratorEmitAbsorber[F[_],C<:CpsMonadContext[F],T](using ec: ExecutionContext, auxAsyncMonad: CpsConcurrentMonad.Aux[F,C]) extends CpsAsyncEmitAbsorber4[AsyncIterator[F,T],F,C,T]:

  override type Element = T
  override type Context = C

  override val asyncMonad = auxAsyncMonad

  override def eval(f: C => CpsAsyncEmitter[Monad,Element] => Monad[Unit]): AsyncIterator[F,T] =
     val list = AsyncListEmitAbsorber[F,C,T].eval(f)
     AsyncListIterator(list)



