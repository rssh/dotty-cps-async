package cps.macros.forest

import cps.macros._

trait AsyncTreeShifter[F[_], CT]:

   thisTreeTransform: TreeTransformScope[F, CT] =>

   import qctx.reflect._

   
   def asyncShift(t:Term, shiftedSymbols:Set[Symbol]):Term =
    t match 
      case Block(stats,last) => Block(stats.map(asyncShiftStatement(_,shiftedSymbols)),
                                      asyncShift(last,shiftedSymbols))
      case If(cond, thenp, elsep) => If(asyncShift(cond, shiftedSymbols),
                                        asyncShift(thenp, shiftedSymbols),
                                        asyncShift(elsep, shiftedSymbols))
      case Match(scrutinee, cases) => Match(asyncShift(scrutinee, shiftedSymbols),
                                            cases.map(asyncShiftCaseDef(_,shiftedSymbols)) )
      case While(cond, body) => While(asyncShift(cond, shiftedSymbols),
                                      asyncShift(body, shiftedSymbols))
      case Try(body, cases, finalizer) => Try(asyncShift(body, shiftedSymbols),
                                             cases.map(asyncShiftCaseDef(_, shiftedSymbols) ),
                                             finalizer.map(asyncShift(_,shiftedSymbols)))
      case _ =>
              ???
  
   def asyncShiftStatement(t: Statement, shiftedSymbols: Set[Symbol]):Statement =
    t match
       case x: Definition => asyncShiftDefinition(x, shiftedSymbols)
       case y: Term => asyncShift(y, shiftedSymbols)

   def asyncShiftDefinition(t: Definition, shiftedSymbols: Set[Symbol]):Definition = ???

   def asyncShiftCaseDef(cd: CaseDef, shiftedSymbols: Set[Symbol]):CaseDef = ???

