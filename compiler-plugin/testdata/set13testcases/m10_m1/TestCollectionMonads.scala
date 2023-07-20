package cpstest

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.util.*

import cps.*
import cps.monads.{*,given}
import cps.plugin.annotation.*
import cps.macros.flags.UseCompilerPlugin
given UseCompilerPlugin.type = UseCompilerPlugin



class TestCollectionMonads {



    @Test
    def testThrowInsideCollection() = {
      val l = Seq(1,2,3)
        reify[Seq] {
          val v = reflect(l)
          if (v==2) then
            throw RuntimeException("v==2")
        }
    }


}