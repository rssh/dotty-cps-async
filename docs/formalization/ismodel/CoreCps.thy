theory CoreCps
  imports Main
begin




datatype  typeexpr = AnyTp
     |
      ConstTp
     |
      ArrowTp typeexpr typeexpr
     |
      MonadTp typeexpr
    |
      ErrorTp


(* TODO: use typeclasse s*)
fun isCorrectTp :: "typeexpr \<Rightarrow> bool" where
  "isCorrectTp AnyTp = True"
 |"isCorrectTp ConstTp = True"
 |"isCorrectTp (ArrowTp t1 t2) = ((isCorrectTp t1)\<and>(isCorrectTp t2))"
 |"isCorrectTp (MonadTp t) = isCorrectTp t"
 |"isCorrectTp ErrorTp = False"




datatype expr = ConstantExpr int
  |
   Let int typeexpr expr expr
  |
   Ref int typeexpr 
  |
   If expr expr expr
  |
   While expr expr 
  |
   Lambda int typeexpr  expr 
  |
   App expr expr
 |
   Mpure expr
 |
   MflatMap expr expr
 |
   Await expr
 |
   ExternalFun int typeexpr
 |
   Error string




fun lub :: "typeexpr \<Rightarrow> typeexpr \<Rightarrow> typeexpr" where
   "lub ConstTp ConstTp = ConstTp"
  |
   "lub (ArrowTp a1 b1) (ArrowTp a2 b2) = 
    (if (a1 = a2) then (ArrowTp a1 (lub b1 b2)) else AnyTp)
   " 
  |
   "lub ErrorTp x = ErrorTp"
  |
   "lub x ErrorTp = ErrorTp"
  |
   "lub AnyTp x = (if (x = ErrorTp) then ErrorTp else AnyTp)" 
  |
   "lub x AnyTp = (if (x = ErrorTp) then ErrorTp else AnyTp)"
  |
   "lub ConstTp x = AnyTp"
  |
   "lub (ArrowTp x y) z = AnyTp"
 |
   "lub (MonadTp x) (MonadTp y) = MonadTp (lub x y)"
 |
   "lub (MonadTp x) y = AnyTp"

type_synonym varIndex = int
type_synonym typeVarState = "varIndex \<Rightarrow> typeexpr"


fun typeExpr :: "expr \<Rightarrow> typeVarState \<Rightarrow> typeexpr" where
    "typeExpr (ConstantExpr x) s = ConstTp"
  |
    "typeExpr (Let v vt vv body) s = (typeExpr body (s(v:=vt)) )"
  |
    "typeExpr (Ref i it) s = (if (it = s(i)) then it else ErrorTp)"
  |
    "typeExpr (If cond ifTrue ifFalse) s = (
        case (typeExpr cond s) of
          ConstTp \<Rightarrow> (lub (typeExpr ifTrue s) (typeExpr ifFalse s))
          | other \<Rightarrow> ErrorTp 
    )"
  |
    "typeExpr (While e1 e2) s = ConstTp"
  |
    "typeExpr (Lambda  i tp body) s = (ArrowTp tp (typeExpr body (s(i:=tp))))"
  |
    "typeExpr (App x arg) s =
                (case (typeExpr x s) of
                     ErrorTp \<Rightarrow> ErrorTp
                    |
                     (ArrowTp xi xo) \<Rightarrow> if ((typeExpr arg s) = xi) then xo else ErrorTp
                    |
                     other \<Rightarrow> ErrorTp
                )  
    "
  |
    "typeExpr (Mpure x) s = ( case (typeExpr x s) of 
                                 ErrorTp \<Rightarrow> ErrorTp
                               | tx \<Rightarrow> MonadTp tx
                            )"
  |
    "typeExpr (MflatMap fa f) s = (
        case (typeExpr fa s) of
            MonadTp a \<Rightarrow> (case (typeExpr f s) of
                            ArrowTp a1 a2 \<Rightarrow> ConstTp
                          | other \<Rightarrow> ErrorTp)
          | other \<Rightarrow> ErrorTp
    )"
  |
    "typeExpr (Await e) s = (
       case (typeExpr e s) of
         MonadTp te \<Rightarrow> te
        | other     \<Rightarrow> ErrorTp
    )"
  |
    "typeExpr (ExternalFun fi ft) s = ft"
  | 
    "typeExpr (Error e) s = ErrorTp"
  


datatype AsyncKind = AKSync | AKAsync AsyncKind | AKLambda AsyncKind | AKError

fun isCorrectAk :: "AsyncKind \<Rightarrow> bool" where
   " isCorrectAk AKSync = True "
 | " isCorrectAk (AKAsync k) = (isCorrectAk k) "
 | " isCorrectAk (AKLambda k) = (isCorrectAk k)"
 | " isCorrectAk AKError = False "

fun isCompatibleAk :: "AsyncKind \<Rightarrow> AsyncKind \<Rightarrow> bool" where
   "isCompatibleAk AKSync AKSync = True"
 |
   "isCompatibleAk AKSync (AKAsync k2) = isCompatibleAk AKSync k2"
 |
   "isCompatibleAk AKSync (AKLambda k2) = False"
 |
   "isCompatibleAk AKSync AKError = False"
 |
   "isCompatibleAk (AKAsync k1) AKSync = isCompatibleAk k1 AKSync"
 |
   "isCompatibleAk (AKAsync k1) (AKAsync k2) = isCompatibleAk k1 k2"
 |
   "isCompatibleAk (AKAsync k1) (AKLambda k2) = False"
 |
   "isCompatibleAk (AKAsync k1) AKError = False"
 |
   "isCompatibleAk (AKLambda k1) AKSync = False"
 |
   "isCompatibleAk (AKLambda k1) (AKAsync k2) = False"
 |
   "isCompatibleAk (AKLambda k1) (AKLambda k2) = isCompatibleAk k1 k2"
 |
   "isCompatibleAk AKError x = False"
 |
   "isCompatibleAk x  AKError = False"


datatype CpsTree =
        CpsTreePure expr
        |
        CpsTreeFlatMap CpsTree int typeexpr CpsTree
        |
        CpsTreeAsync expr AsyncKind
        |
        CpsTreeLambda int typeexpr CpsTree
        |
         (* analog of SeqCpsTree *)
        CpsTreeLet int typeexpr expr CpsTree 
        |
        ErrorCpsTree

fun typeCpsTree:: "CpsTree \<Rightarrow> typeVarState \<Rightarrow> typeexpr" where
       "typeCpsTree (CpsTreePure expr) s = MonadTp (typeExpr expr s)"
     |
       "typeCpsTree (CpsTreeFlatMap src i tb fbody) s = (
          case (typeCpsTree src s) of
            MonadTp srct \<Rightarrow> 
                (case (typeCpsTree fbody (s(i:=srct))) of
                    ArrowTp fx fy \<Rightarrow> if (fx = srct) then fy else ErrorTp
                   | other \<Rightarrow> ErrorTp)
          |other \<Rightarrow> ErrorTp
       )"
     |
       "typeCpsTree (CpsTreeAsync e k) s = (
           (typeExpr e s) 
       )"
     |
       "typeCpsTree (CpsTreeLambda v vt body) s = (
         case (typeCpsTree body (s(v:=vt))) of
            ErrorTp \<Rightarrow> ErrorTp
           | rt     \<Rightarrow> ArrowTp vt rt
       )"
     |
       "typeCpsTree (CpsTreeLet v vt init body) s = (
          if (vt = ErrorTp) then ErrorTp else typeCpsTree body (s(v:=vt)) 
       )"
     |
       "typeCpsTree ErrorCpsTree s = ErrorTp" 


fun isCorrectExpr::"expr \<Rightarrow> typeVarState \<Rightarrow> bool" where
       "isCorrectExpr e s = ((typeExpr e s) \<noteq> ErrorTp)" 

fun isCorrectCps :: "CpsTree \<Rightarrow> typeVarState \<Rightarrow> bool" where
   "isCorrectCps t s = ((typeCpsTree t s) \<noteq> ErrorTp)"
 

fun asyncKind :: "CpsTree \<Rightarrow> AsyncKind" where
    "asyncKind (CpsTreePure e) = AKSync"
  |
    "asyncKind (CpsTreeAsync e k) = AKAsync k"
  |
    "asyncKind (CpsTreeFlatMap source i tp fbody) = (
       case (asyncKind source) of
            (AKSync) \<Rightarrow> AKAsync (asyncKind fbody)
          | (AKAsync Sync) \<Rightarrow> AKAsync (asyncKind fbody) 
          | other \<Rightarrow> AKError
    )"
  |
   (* here is problem because lambda cab have 2 possible cps-ed representations:
      Lambda: A\<Rightarrow>F[B]  and Pure: F[A\<Rightarrow>B] which is sometimes possible. (when body is sync)
      For now we follow scala implementation, return lambda and check for possinility of
      sync representation during transformation.
    *)
    "asyncKind (CpsTreeLambda i tp body) = AKLambda (asyncKind body)"
  |
    "asyncKind (CpsTreeLet v vt vv cpsBody) = asyncKind cpsBody"
  |
    "asyncKind ErrorCpsTree = AKError"
    
definition isRedusableCps::"CpsTree \<Rightarrow> typeVarState \<Rightarrow> bool" where
   "isRedusableCps t s \<longleftrightarrow> (isCorrectCps t s) \<and> ((asyncKind t) \<noteq> AKError)"


(*
 analog of CpsTree.transformed
*)
fun cpsTreeToExpr :: "CpsTree \<Rightarrow> typeVarState \<Rightarrow> expr" where 
   "cpsTreeToExpr (CpsTreePure e) s = Mpure e"
 |
   "cpsTreeToExpr (CpsTreeFlatMap arg v vt vbody) s = 
                   MflatMap (cpsTreeToExpr arg s) (Lambda v vt (cpsTreeToExpr vbody (s(v:=vt))))"
 |
   "cpsTreeToExpr (CpsTreeAsync e k) s = (
       case k of
           AKSync \<Rightarrow> e
         | AKAsync k1 \<Rightarrow> (
             case (typeExpr e s) of
                 (MonadTp et) \<Rightarrow> if (isCorrectTp et) then 
                                    (MflatMap e (Lambda 1 et (Ref 1 et)))
                                 else (Error ''AAA'')
                 |other \<Rightarrow> Error ''Invalud async kind''
           )
         | other \<Rightarrow> Error ''Invalid async kind''
   )"   (* TODO: handle AsynK*(Sync)  *)
 |
   "cpsTreeToExpr (CpsTreeLambda v vt vbody) s = (Lambda v vt (cpsTreeToExpr vbody (s(v:=vt))))"
 |
   "cpsTreeToExpr (CpsTreeLet v vt vv cpsBody) s = (Let v vt vv (cpsTreeToExpr cpsBody (s(v:=vt))))"
  |
   "cpsTreeToExpr ErrorCpsTree s = Error ''ErrorCpsTree'' "
 


fun unpureCpsTree :: "CpsTree \<Rightarrow> (expr option)" where
   "unpureCpsTree (CpsTreePure e) =  (Some e)"
  | 
   "unpureCpsTree (CpsTreeFlatMap source i tp fbody) = (
       case (unpureCpsTree source) of 
         None \<Rightarrow> None
        |
         Some(unpureSource) \<Rightarrow>
           (case (unpureCpsTree fbody) of
             None \<Rightarrow> None
            |
             Some unpureFbody \<Rightarrow> Some (Let i tp (unpureSource) (unpureFbody) )
           )
   )"
  |
   "unpureCpsTree (CpsTreeAsync e k) = None"
  |
   "unpureCpsTree (CpsTreeLambda i tpi cpsBody) = (
      case (unpureCpsTree cpsBody) of
         Some body \<Rightarrow> Some (Lambda i tpi body)
        |
         None \<Rightarrow> None
   )"
 |
  "unpureCpsTree (CpsTreeLet v vt vv cpsBody) = (
      case (unpureCpsTree cpsBody) of
        Some body \<Rightarrow> Some (Let v vt (vv) (body))
        |
        None \<Rightarrow> None 
  )"
 |
  "unpureCpsTree ErrorCpsTree = None" 

lemma correctNotError: "isCorrectCps (t::CpsTree) s \<Longrightarrow> t \<noteq> ErrorCpsTree"
  by auto

lemma correctNoErrorKind: "(isCorrectCps (t::CpsTree) (s::typeVarState)) \<Longrightarrow> (asyncKind t) \<noteq> AKError"
  (*nitpick*)
  
  oops
  



lemma letUnputAsBody: " 
    (isCorrectTp (vt::typeexpr))
    \<and>(isCorrectExpr (vv::expr) (s::typeVarState))
    \<and>(isCorrectCps (cpsBody::CpsTree) s)
    \<and>((unpureCpsTree cpsBody) \<noteq> None) \<Longrightarrow>
        (unpureCpsTree (CpsTreeLet (v::int) vt vv cpsBody)) \<noteq> None"
  (*nitpick *)
  apply auto
  done

lemma flatMapUnputAsBody: "
   (isCorrectCps (source::CpsTree) (s::typeVarState))
   \<and>
   ((unpureCpsTree source) \<noteq> None)
   \<and>
   (isCorrectTp (vt::typeexpr))
   \<and>
   (isCorrectCps (fbody::CpsTree) s)
   \<and>
   ((unpureCpsTree fbody) \<noteq> None)
           \<Longrightarrow>
           unpureCpsTree((CpsTreeFlatMap source v vt fbody) ) \<noteq> None
"
  apply auto
  done

lemma syncHaveUnpure: "(isCorrectCps (t::CpsTree) (s::typeVarState) )\<and>(asyncKind(t) = AKSync) \<Longrightarrow> unpureCpsTree(t) \<noteq> None"
  nitpick
  apply(induct t)
  apply auto
  apply (smt (verit, del_insts) AsyncKind.distinct(1) AsyncKind.simps(15) AsyncKind.simps(16) AsyncKind.simps(17) AsyncKind.simps(18) isCorrectAk.cases)
  sorry


(*
 CpsTransform 
*)
fun exprToCpsTree :: "expr \<Rightarrow> typeVarState \<Rightarrow> CpsTree" where
    "exprToCpsTree (ConstantExpr c) s =  CpsTreePure (ConstantExpr c)"
  |
    "exprToCpsTree (Ref i tp) s = CpsTreePure (Ref i tp)"
  |
    "exprToCpsTree (Let v vt vv body) s = (
        let cpsv = (exprToCpsTree vv s) in (
         let cpsbody = (exprToCpsTree body s) in
           (case (unpureCpsTree cpsv) of
             Some pureV \<Rightarrow> 
               (case (unpureCpsTree cpsbody) of
                   Some pureBody \<Rightarrow> CpsTreePure (Let v vt (pureV) (pureBody))
                  |None \<Rightarrow> CpsTreeLet v vt (pureV) cpsbody
               )
             |None \<Rightarrow>
                (case (asyncKind cpsv) of
                    AKAsync ik \<Rightarrow> 
                     (CpsTreeFlatMap (cpsv) v vt (cpsbody))
                    | AKLambda bk \<Rightarrow> ErrorCpsTree
                )
           )
        )
    )"
  |
   (* TODO *)
   "exprToCpsTree (If c e1 e2) s = (
    let cpsC = (exprToCpsTree c s); 
        cpsE1 = (exprToCpsTree e1 s);
        cpsE2 = (exprToCpsTree e2 s) in (
       ErrorCpsTree 
    )
   )"
    
    

end
