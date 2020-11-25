package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

object T1 {

  def f1(): Unit = { }

  def f2(): Unit = { }

  def cb(): ComputationBound[Unit] = Done(())

  def cbi(n:Int): ComputationBound[Int] = Done(n)

  def cbBool(b:Boolean): ComputationBound[Boolean] = Done(b)

  def cbs(s:String): ComputationBound[String] = Done(s)

  def cbt[T](v:T): ComputationBound[T] = Done(v)

}


