package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

object T1 {

  def f1(): Unit = { }

  def f2(): Unit = { }

  def cb(): ComputationBound[Unit] = Done(())

  def cbi(n:Int): ComputationBound[Int] = Done(n)

  def cbBool(b:Boolean): ComputationBound[Boolean] = Done(b)

}

