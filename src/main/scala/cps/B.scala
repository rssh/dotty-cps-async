package cps

import scala.quoted._
import scala.quoted.matching._


object B {



 def badTree[T](implicit given qctx: QuoteContext)(q:Expr[T]):Expr[CB[T]] = {
  val identCBA = findIdent(ComputationBoundAsyncMonad)
  val selectCBAFlatMap = findSelect(identCBA,"flatMap")
  val tptBoolean = StandardTypes.BooleanType.unseal
  val tptInt = StandardTypes.IntType.unseal
  val t = Inlined(
   None,List(),
   Apply(
    Apply(
     TypeApply(
      //Select(Inlined(None,List(),identCBA),flatMap),
      selectCBAFlatMap,
      List(tptBoolean, tptInt)
     ),
     List(
      //Inlined(None,List(),
      // Apply(Select(Ident(T1),cbBool),List(Literal(Constant(true)))))
      '{ T1.cbBool(true) }.unseal
     )
    ),
    List(
     '{ v => 
       ${
          
       }
     }
     Block(
      List(
       DefDef($anonfun,List(),
        List(
         List(
          ValDef(v,TypeTree[Boolean],EmptyTree)
        )),
        TypeTree[AppliedType(ComputationBound,
                           List(Int))],
        Inlined(None,List(),
         Block(
          List(
           ValDef(x,TypeTree[Boolean],Ident(v)), 
           Literal(Constant(())), 
           ValDef(y,TypeTree[Int],Literal(Constant(3))), 
           Literal(Constant(()))
          ),
          Inlined(
           Ident(ValDefTransform$),List(),
           Apply(
            Apply(
             TypeApply(
              Select(Inlined(EmptyTree,List(),Ident(ComputationBoundAsyncMonad)),map),
              List(TypeTree[Int], TypeTree[Int])
             ),
             List(
              Inlined(EmptyTree,List(),
               Apply(Select(Ident(T1),cbi),List(Literal(Constant(2))))
              )
             )
            ),
            List(
             Block(
              List(
               DefDef($anonfun,List(),
                List(
                 List(ValDef(v,TypeTree[Int],EmptyTree))
                ),
                TypeTree[Int],
                Inlined(EmptyTree,List(),
                 Block(
                  List(ValDef(z,TypeTree[Int],Ident(v)), 
                     Literal(Constant(()))
                  ),
                  Apply(Select(Ident(y),+),List(Ident(z)))
                 )
                )
               )
              ),
              Closure(List(),Ident($anonfun),EmptyTree)
             )
            )
           )
          )
         )))),Closure(List(),Ident($anonfun),EmptyTree))))
  )

 }
}
