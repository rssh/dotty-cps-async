package cps


class TestBS1If


  def tBadTree(): Unit = 
     val c = B.badTree[Int]{
       { val x :scala.Boolean = B.await(T1.cbBool(true))
         val y :scala.Int = 3
         val z :scala.Int = B.await(T1.cbi(2))
         y+z
       }
     }


