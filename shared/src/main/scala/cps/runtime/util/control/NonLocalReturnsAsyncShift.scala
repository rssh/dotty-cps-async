package cps.runtime.util.control

import scala.util.*
import scala.util.control.*
import scala.util.control.NonLocalReturns.*
import cps.*


object NonLocalReturnsAsyncShift extends AsyncShift[NonLocalReturns.type] {


   def returning[F[_],T](o:NonLocalReturns.type,m:CpsTryMonad[F])(op: ReturnThrowable[T] => F[T]): F[T] = {
      val rt  = ReturnThrowable[T]()
      try 
        m.flatMapTry(op(rt)){ 
          case Success(x) => m.pure(x)
          case Failure(ex) =>
            if (ex.isInstanceOf[ControlThrowableAsyncWrapper]) then
              val cex = ex.asInstanceOf[ControlThrowableAsyncWrapper]
              if (cex.ce eq rt) then
                m.pure(rt.result)
              else
                m.error(cex)
            else
              m.error(ex)
        }
      catch
        case w: ControlThrowableAsyncWrapper if w.ce eq rt =>
          m.pure(rt.result)  
        case NonFatal(ex) =>
          m.error(ex)
        case ct: ReturnThrowable[?] =>
          if (ct eq rt) then
            m.pure(rt.result)
          else
            m.error(ControlThrowableAsyncWrapper(ct))
   }

   def throwReturn[T](r:T)(rt: ReturnThrowable[T]): Nothing = {
       try {
          rt.throwReturn(r)
       } catch {
          case e: ReturnThrowable[?] if e eq rt =>
            throw new ControlThrowableAsyncWrapper(rt)
       }
   }

   /**
    * Version of returning,
    **/
   def syncReturning[T](op:ReturnThrowable[T] => T):T = {
     val returner = new ReturnThrowable[T]()
     try 
       op(returner)
     catch
       case ex: ReturnThrowable[?] if ex eq returner =>
              returner.result
       case ctex: ControlThrowableAsyncWrapper if ctex.ce eq returner =>
              returner.result  
   }


}

