package logic

import cps.*
import cps.monads.{*, given}
import logict.{*,given}

import org.junit.Test


class GrandParentsTest {

  @Test
  def testLogicM() = {
     import GrandParentsTest.*
     val r = grandParent[LogicM]("Anne").observeAll
     println(s"GrandParentTest: r=$r")
     assert(r.size == 2)
     assert(r.contains("Sarah"))
     assert(r.contains("Arnold"))
  }



}

object GrandParentsTest {

   case class IsParentOf(parent:String, child:String)

   def parents = Seq(
     IsParentOf("Sarah", "John"),
     IsParentOf("Arnold", "John"),
     IsParentOf("John", "Anne"),
   )

   def grandParent[M[_]:CpsLogicMonad](name:String):M[String] = reify[M]{
       // TODO: fill bug report in dotty.
       //  shopuld be search
       import cps.CpsMonadConversion.given
       val IsParentOf(p,c) = reflect(all(parents))
       guard(c == name)
       val IsParentOf(gp,_) = reflect(all(parents.filter(_.child == p)))
       gp
   }

}