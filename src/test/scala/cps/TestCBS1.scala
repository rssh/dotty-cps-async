package cps


class TestBS1

  def testBadTree(): Unit =
    val c = B.badTree[Int]{
         val x1 = 3
         CBM.pure(x1 + 4)
    }



