package cps.runtime

import cps.*

object WhileHelper {

    def  w11[F[_]](m:CpsMonad[F], cond: =>F[Boolean], repeat: =>F[Unit]):F[Unit] =
      m.flatMap(cond){ c =>
        if (c) then
          m.flatMap(repeat)( u => w11(m, cond,repeat) )
        else 
          m.pure(())
      }


    def  w10[F[_]](m:CpsMonad[F], cond: =>F[Boolean], repeat: => Unit):F[Unit] =
      m.flatMap(cond){ c =>
        if (c) {
           repeat
           w10(m, cond,repeat)
        } else {
           m.pure(())
        }
      }
 
    def  w01[F[_]](m:CpsMonad[F], cond: =>Boolean, repeat: => F[Unit]):F[Unit] =
        if (cond) {
          m.flatMap(repeat)( u => w01(m,cond,repeat))
        } else {
          m.pure(())         
        }
        
}