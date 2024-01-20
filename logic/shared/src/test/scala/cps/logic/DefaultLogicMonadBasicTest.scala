package cps.logic

import org.junit.{Test,Ignore}

import cps.*
import cps.monads.logic.*

class DefaultLogicMonadBasicTest {

  @Test
  def logicMonadFromCollection() = {
    val m = LogicStream.fromCollection(List(1,2,3))
    
  }



}


