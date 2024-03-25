package cps.logic

import org.junit.{Test,Ignore}

import cps.*
import cps.monads.logic.*

/**
 * Someone in Dreadsbury Mansion killed Aunt Agatha.
  *Agatha
*, the butler
*, and Charles live in Dreadsbury Mansion
*, and
* are the only ones to live there.
* (+)A killer always hates and is
* (+) no richer than his victim.
* (+)Charles hates noone that Agatha hates
*.
*(+)Agatha hates everybody except the butler
*.(+) The butler hates everyone
  *not richer than Aunt Agatha.
 * (+) The butler hates everyone whom Agatha hates.
  *(+) Noone hates everyone.
 * Who killed Agatha ?
**/

class AgadaTest {

   @Test def testAgada(): Unit = {
      val worlds = AgadaTest.allWorlds()
      val result = worlds.toLazyList.toIndexedSeq
      val killer = result.map(_.killer).toSet
      //for { r <- result } {
      //  println(s"result.killer=${r.killer}")
      //  println(s"result.richer=${r.richer}")
      //  println(s"result.hates=${r.hates}")
      //  println("------------------")
      //}
      assert(killer.size == 1)
      assert(killer.head == AgadaTest.agata)
   }

}

object AgadaTest {

   case class Person(name: String)
   case class Richer(p1: Person, p2: Person)
   case class Hates(p1: Person, p2: Person)

   val agata = Person("Agatha")
   val butler = Person("Butler")
   val charles = Person("Charles")

   val citizents = List(agata,butler,charles)

   case class World(
      richer: Set[Richer],
      hates: Set[Hates],
      killer: Person
   )

   def allWorlds(): LogicStream[World] = reify[LogicStream]{
      val richer = reflect(allRichers())
      val hates = reflect(allHates())
      val killer = reflect(LogicStream.fromCollection(citizents))

      // a killer alwasy hates. ... victim

      guard {
        hates.contains(Hates(killer,agata))
      }

      guard {
        // a killer is no richer than his victim
        !richer.contains(Richer(killer,agata))
      }


      //The butler hates everyone
      //  * not richer than Aunt Agatha
      guard {
          citizents.forall(p =>
            if (richer.contains(Richer(agata,p))) {
              hates.contains(Hates(butler,p))
            } else {
              true
            }
          )
      }

      World(richer, hates, killer)
   }


   def allRichers(): LogicStream[Set[Richer]] = reify[LogicStream]{
      val allRicher = for(p1 <- citizents; p2 <- citizents if p1!=p2) yield Richer(p1,p2)
      val candidate = reflect(allSubsets(allRicher.toIndexedSeq)).toSet
      guard(candidate.forall{ case Richer(p1,p2) => !candidate.contains(Richer(p2,p1)) })
      guard{
        // note, that if we assume, that all wealth are different.
        //  if we don't assume this, other solutions become possible
        val pairs = for{ p1 <- citizents; p2 <- citizents if p1!=p2 } yield (p1,p2)
        pairs.forall{ case (p1,p2) =>
          candidate.contains(Richer(p1,p2)) || candidate.contains(Richer(p2,p1))
        }
      }
      candidate
   }

   def allHates(): LogicStream[Set[Hates]] = reify[LogicStream]{
      def allPairs = for(p1 <- citizents; p2 <- citizents) yield Hates(p1,p2)
      val candidate = reflect(allSubsets(allPairs.toIndexedSeq)).toSet

      // Agatha hates everybody except the butler
      guard(
        !candidate.contains(Hates(agata,butler)) &&
         candidate.contains(Hates(agata,agata)) &&
         candidate.contains(Hates(agata,charles))
      )

      // Charles hates noone that Agatha hates
      // The butler hates everyone whom Agatha hates.
      guard(
        citizents.forall{ p =>
          if (candidate.contains(Hates(agata,p))) {
            !candidate.contains(Hates(charles,p))
            &&
            candidate.contains(Hates(butler,p))
          } else {
            true
          }
        }
      )



      //Noone hates everyone.
      guard{
        citizents.forall(p => !citizents.forall(p1 => candidate.contains(Hates(p,p1))))
      }

      candidate
   }

   def allSubsets[T](xs: IndexedSeq[T], from:Int=0): LogicStream[List[T]] = reify[LogicStream]{
      if (xs.size == from) {
        List.empty
      } else {
        val head = xs(from)
        val tail = reflect(allSubsets(xs,from+1))
        val next = LogicStream.pure(head::tail) |+| LogicStream.pure(tail)
        reflect(next)
      }
   }


}