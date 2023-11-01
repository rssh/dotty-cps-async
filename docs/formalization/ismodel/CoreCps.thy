theory CoreCps
  imports Main
begin


datatype  TpExpr = AnyTp
     |
      ConstTp
     |
      ArrowTp TpExpr TpExpr
     |
      MonadTp TpExpr
    |
      ErrorTp


(* TODO: use typeclasse s*)
fun isCorrectTp :: "TpExpr \<Rightarrow> bool" where
  "isCorrectTp AnyTp = True"
 |"isCorrectTp ConstTp = True"
 |"isCorrectTp (ArrowTp t1 t2) = ((isCorrectTp t1)\<and>(isCorrectTp t2))"
 |"isCorrectTp (MonadTp t) = isCorrectTp t"
 |"isCorrectTp ErrorTp = False"




datatype Expr = ConstantExpr int
  |
   Let int TpExpr Expr Expr
  |
   Ref int TpExpr 
  |
   If Expr Expr Expr
  |
   While Expr Expr  (* TODO: introduce after extending type to unit *)
  |
   Lambda int TpExpr Expr 
  |
   App Expr Expr
 |
   Mpure Expr
 |
   MflatMap Expr Expr
 |
   Await Expr
 |
   ExternalFun int TpExpr
 |
   Error string




fun lub :: "TpExpr \<Rightarrow> TpExpr \<Rightarrow> TpExpr" where
   "lub ConstTp ConstTp = ConstTp"
  |
   lub_ArrowTpArrowTp: "lub (ArrowTp a1 b1) (ArrowTp a2 b2) = 
    (if (isCorrectTp a1)\<and>
        (isCorrectTp a2)\<and> 
        (isCorrectTp b1)\<and>
        (isCorrectTp b2) then (
          if (a1 = a2) then (ArrowTp a1 (lub b1 b2)) 
                       else AnyTp
         ) else ErrorTp
   )" 
  |
   "lub ErrorTp y = ErrorTp"
  |
   "lub x ErrorTp = ErrorTp"
  |
   "lub AnyTp x = (if (isCorrectTp x) then AnyTp else ErrorTp)" 
  |
   "lub x AnyTp = (if (isCorrectTp x) then AnyTp else ErrorTp)"
  |
   "lub ConstTp x = (if (isCorrectTp x) then AnyTp else ErrorTp)"
  |
   "lub (ArrowTp x y) z = (
        if (isCorrectTp x)\<and>(isCorrectTp y)\<and>(isCorrectTp z) 
            then AnyTp
        else ErrorTp)"
 |
   "lub (MonadTp x) (MonadTp y) = (
                if (isCorrectTp x)\<and>(isCorrectTp y) 
                   then MonadTp (lub x y) else ErrorTp 
  )"
 |
   "lub (MonadTp x) y = (if (isCorrectTp x)\<and>(isCorrectTp y) then AnyTp else ErrorTp)"


type_synonym VarIndex = int
type_synonym TpVarState = "VarIndex \<Rightarrow> TpExpr"

definition isCorrectVarType::" int \<Rightarrow> TpVarState \<Rightarrow> bool" where
   "isCorrectVarType x s = ((s(x)) \<noteq> ErrorTp)"


fun maxVarIndex:: "Expr \<Rightarrow> int \<Rightarrow> int" where
   "maxVarIndex (ConstantExpr x) a = a"
 | "maxVarIndex (Let i tp init body) a = (maxVarIndex body (maxVarIndex init (max a i)))"
 | "maxVarIndex (Ref i tp) a = max a i"
 | "maxVarIndex (If c e1 e2) a = (maxVarIndex e2 (maxVarIndex e1 (maxVarIndex c a)))"
 | "maxVarIndex (While e1 e2) a = (maxVarIndex e1 (maxVarIndex e2 a))"
 | "maxVarIndex (Lambda i tp e) a = (maxVarIndex e (max a i))"
 | "maxVarIndex (App e1 e2) a = (maxVarIndex e1 (maxVarIndex e2 a))"
 | "maxVarIndex (Mpure e) a = maxVarIndex e a"
 | "maxVarIndex (MflatMap e1 e2) a = maxVarIndex e1 (maxVarIndex e2 a)"
 | "maxVarIndex (Await e) a = maxVarIndex e a"
 | "maxVarIndex (ExternalFun i tp) a = a"
 | "maxVarIndex (Error s) a = a"


fun typeExpr :: "Expr \<Rightarrow> TpVarState \<Rightarrow> TpExpr" where
    "typeExpr (ConstantExpr x) s = ConstTp"
  |
    "typeExpr (Let v vt vv body) s = (
        if \<not> (isCorrectTp vt) then ErrorTp else
         (case (typeExpr vv s) of
           ErrorTp \<Rightarrow> ErrorTp
           | vvt \<Rightarrow> if (vvt \<noteq> vt) then ErrorTp else 
                                   (typeExpr body (s(v:=vt)) )
         )
    )"
  |
    "typeExpr (Ref i it) s = ( 
               if \<not>(isCorrectTp it) then ErrorTp
                 else
                   if ((s(i)) = it) then it else ErrorTp
            )"
  |
    "typeExpr (If cond ifTrue ifFalse) s = (
        case (typeExpr cond s) of
          ConstTp \<Rightarrow> (lub (typeExpr ifTrue s) (typeExpr ifFalse s))
          | other \<Rightarrow> ErrorTp 
    )"
  |
    "typeExpr (While e1 e2) s = ConstTp"
  |
    "typeExpr (Lambda i tp body) s = (
      if \<not>(isCorrectTp tp) then
         ErrorTp
       else (case (typeExpr body (s(i:=tp))) of
                  ErrorTp \<Rightarrow> ErrorTp
                 |
                  tb \<Rightarrow> ArrowTp tp tb 
              )
    )"
  |
    "typeExpr (App x arg) s =
                (
                  case (typeExpr x s) of
                     (ArrowTp xi xo) \<Rightarrow> (
                           if (isCorrectTp xi)\<and>((typeExpr arg s) = xi) then
                             xo
                           else ErrorTp
                         )
                    |
                     other \<Rightarrow> ErrorTp
                )  
    "
  |
    "typeExpr (Mpure x) s = ( let tx = (typeExpr x s) in 
                                if (isCorrectTp tx) then tx else ErrorTp
                            )"
  |
    "typeExpr (MflatMap fa f) s = (
        let at = (typeExpr fa s); ft = (typeExpr f s) in
          if (isCorrectTp at)\<and>(isCorrectTp ft) then
            (case at of
               MonadTp a \<Rightarrow> (case ft of
                                 ArrowTp a1 (MonadTp a2) \<Rightarrow> a2
                               | other \<Rightarrow> ErrorTp)
               | other \<Rightarrow> ErrorTp
            )
          else ErrorTp
   )"
  |
    "typeExpr (Await e) s = (
       case (typeExpr e s) of
         MonadTp te \<Rightarrow> if (isCorrectTp te) then te else ErrorTp
        | other     \<Rightarrow> ErrorTp
    )"
  |
    "typeExpr (ExternalFun fi ft) s = (if (isCorrectTp ft) then ft else ErrorTp)"
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
        CpsTreePure Expr
        |
        CpsTreeFlatMap CpsTree int CpsTree
        |
        CpsTreeAsync Expr AsyncKind
        |
        CpsTreeLambda int TpExpr CpsTree
        |
         (* analog of SeqCpsTree *)
        CpsTreeLet int TpExpr Expr CpsTree 
        |
        ErrorCpsTree

fun errorCpsTree:: "string \<Rightarrow> CpsTree" where
    "errorCpsTree message = ErrorCpsTree"

fun typeCpsTree:: "CpsTree \<Rightarrow> TpVarState \<Rightarrow> TpExpr" where
       "typeCpsTree (CpsTreePure expr) s = (case (typeExpr expr s) of
             ErrorTp \<Rightarrow> ErrorTp
            | t \<Rightarrow> MonadTp t
       )"
     |
       "typeCpsTree (CpsTreeFlatMap src i fbody) s = (
          case (typeCpsTree src s) of
           MonadTp srct \<Rightarrow> 
                (case (typeCpsTree fbody (s(i:=srct))) of
                     MonadTp bt \<Rightarrow> if (bt = ErrorTp) then ErrorTp else MonadTp bt
                   | other \<Rightarrow> ErrorTp)
          |other \<Rightarrow> ErrorTp
       )"
     |
       "typeCpsTree (CpsTreeAsync e k) s = (
           case (typeExpr e s) of
              MonadTp t \<Rightarrow> if (t = ErrorTp) then ErrorTp else MonadTp t
             |other   \<Rightarrow> ErrorTp  
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


fun isCorrectExpr::"Expr \<Rightarrow> TpVarState \<Rightarrow> bool" where
       "isCorrectExpr e s = ((typeExpr e s) \<noteq> ErrorTp)" 

fun isCorrectCps :: "CpsTree \<Rightarrow> TpVarState \<Rightarrow> bool" where
   "isCorrectCps t s = ((typeCpsTree t s) \<noteq> ErrorTp)"




lemma isCorrectExpr_let_backward: 
  "isCorrectExpr (Let v vt vv b) s \<Longrightarrow> (isCorrectExpr vv s)\<and>(isCorrectExpr b (s(v:=vt)))"
  (*using [[simp_trace]]*)
  apply(cases "isCorrectTp vt")
   defer
  subgoal
    apply(auto)
    done
  subgoal
    apply(cases "vt = typeExpr vv s")
    defer
    subgoal
      apply(cases "vt")
      apply(auto)
      subgoal
          apply(cases "typeExpr vv s"; auto)
          done
      subgoal
          apply(cases "typeExpr vv s"; auto)
          done
      subgoal
          apply(cases "typeExpr vv s"; auto)
        done
      subgoal
        apply(cases "typeExpr vv s"; auto)
        done
      done
    subgoal
      apply(cases "typeExpr vv s"; auto)
      done
    done
  done

lemma isCorrectExpr_ref_backward:
  "isCorrectExpr (Ref i tp) s \<Longrightarrow> (isCorrectTp tp) \<and> (isCorrectTp (s i))"
  apply(cases "isCorrectTp tp"; auto)
  apply(cases "(s i) = tp"; auto)
  done

lemma lub_x_errorTp: "lub x ErrorTp =  ErrorTp"
  apply(cases "x"; auto)
  done

lemma lub_any_x_errorTp: "\<forall> x. lub x ErrorTp =  ErrorTp"
  (* using [[simp_trace=true]] *)
  apply(auto simp add: lub_x_errorTp)
  done



lemma isCorrectExpr_if_backward:
  "isCorrectExpr (If c e1 e2) s \<Longrightarrow> (isCorrectExpr c s)\<and>(isCorrectExpr e1 s)\<and>(isCorrectExpr e2 s)"
  apply(cases "typeExpr c s"; auto)
  apply(simp add: lub_x_errorTp)
  done

lemma arrowTp_correct_forward: fixes a b :: TpExpr
  assumes "isCorrectTp a" and "isCorrectTp b"
  shows "isCorrectTp (ArrowTp a b)"
proof -
  show ?thesis
    apply(subst isCorrectTp.simps; auto simp add: assms)
    done
qed

lemma arrowTp_correct_backward: fixes a b :: TpExpr
  assumes "isCorrectTp (ArrowTp a b)"
  shows "isCorrectTp a \<and> isCorrectTp b"
using assms proof (cases "isCorrectTp a")
  case False
  from assms show ?thesis by auto
next
  case a_corr:True
  show ?thesis
  proof (cases "isCorrectTp b")
    case b_iv:False
    from assms show ?thesis by auto
  next
    case b_corr:True
    then show ?thesis by(auto simp add: a_corr)
  qed
qed



lemma lub_correct_forward: fixes x y ::TpExpr 
  assumes "(isCorrectTp (x))" and "(isCorrectTp (y))"
  shows  "isCorrectTp (lub x y)"
using assms proof (induction x y rule: lub.induct)
  case 1
  show ?case by auto
next
  case (2 a1 b1 a2 b2)
  then show ?case
  proof (cases "isCorrectTp a1 \<and> isCorrectTp b1\<and> isCorrectTp a2 \<and> isCorrectTp b2")
    case all_correct:True
    then show ?thesis
    proof (cases "a1=a2")
      case True
      from this and all_correct show ?thesis by(auto simp add: 2)
    next
      case False
      from this and all_correct show ?thesis by auto
    qed
  next
    case False
    from this and 2 show ?thesis by auto
  qed
next
  case (3 y)
  then show ?case by auto
next
  case ("4_1")
  then show ?case by auto
next
  case ("4_2")
  then show ?case by auto
next
  case ("4_3" xi xo)
  then show ?case by auto
next
  case ("4_4" mx)
  then show ?case by auto
next
  case ("5_1")
  then show ?case by auto
next
  case ("5_2")
  then show ?case by auto
next
  case ("5_3" xi xy)
  then show ?case by auto
next
  case ("5_4" x)
  then show ?case by auto
next
  case ("6_1")
  then show ?case by auto
next
  case ("6_2" xi xo)
  then show ?case by auto
next
  case ("6_3" x)
  then show ?case by auto
next
  case ("7_1"  yi yo)
  then show ?case by auto
next
  case ("7_2" my)
  then show ?case by auto
next
  case ("8_1" xi xo)
  then show ?case by auto
next
  case ("8_2" xi xo mt)
  then show ?case by auto
next
  case (9 x y)
  then show ?case by auto
next
  case ("10_1" mx)
  then show ?case by auto
next
  case ("10_2" mx xi xo)
  then show ?case by auto  
qed

         
datatype AsyncKindRepr = 
   AKRPure Expr
  |
   AKRAsync AsyncKind CpsTree
 |
   AKRLambda AsyncKind CpsTree
 |
   AKRError


lemma correctFlatMapNoErrorKind: "
    (isCorrectCps (source::CpsTree) s) 
  \<and> ((typeCpsTree source s) = MonadTp ts)
  \<and> (isCorrectCps (body::CpsTree) (s(v:=ts)) ) 
  \<and> ((typeCpsTree body (s(v:=ts))) = MonadTp tb)
                       \<Longrightarrow> isCorrectCps (CpsTreeFlatMap source v body) s"
  nitpick
  apply(auto)
  sorry
  

fun asyncKind :: "CpsTree \<Rightarrow> AsyncKind" where
    "asyncKind (CpsTreePure e) = AKSync"
  |
    "asyncKind (CpsTreeAsync e k) = AKAsync k"
  |
    "asyncKind (CpsTreeFlatMap source i fbody) = (
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




fun complexityAsyncKind::"AsyncKind \<Rightarrow> nat" where
    "complexityAsyncKind  AKSync = 1"
  | "complexityAsyncKind  (AKAsync k) = 1 + (complexityAsyncKind k)"
  | "complexityAsyncKind  (AKLambda k) = 1 + (complexityAsyncKind k)"
  | "complexityAsyncKind  AKError = 0"
  


fun complexityCpsTree::"CpsTree  \<Rightarrow> nat" where
   "complexityCpsTree (CpsTreePure expr) = 1"
 | "complexityCpsTree (CpsTreeFlatMap vsource v vbody) = 
           (complexityCpsTree vsource) + (complexityCpsTree vbody) + 1"
 | "complexityCpsTree (CpsTreeAsync expr ak) = (complexityAsyncKind ak)"
 | "complexityCpsTree (CpsTreeLambda v vt vbody) = 1 + (complexityCpsTree vbody)"
 | "complexityCpsTree (CpsTreeLet v vt init vbody) = 
      1 + (complexityCpsTree vbody)"
 | "complexityCpsTree ErrorCpsTree = 1"




(*
 analog of CpsTree.transformed
*)
function (sequential) cpsTreeToExpr :: "CpsTree \<Rightarrow> TpVarState \<Rightarrow> Expr" where 
   "cpsTreeToExpr (CpsTreePure e) s = Mpure e"
 |
   "cpsTreeToExpr (CpsTreeFlatMap arg v vbody) s = 
        (case (typeCpsTree arg s) of
            MonadTp vt \<Rightarrow>
                   MflatMap (cpsTreeToExpr arg s) 
                            (Lambda v vt (cpsTreeToExpr vbody (s(v:=vt))))
           |other \<Rightarrow> Error ''Invalid CpsTreeFlatMap entry''
       )"
 |
   "cpsTreeToExpr (CpsTreeAsync e k) s = (
       case k of
           AKSync \<Rightarrow> e
         | AKAsync k1 \<Rightarrow> (
             case (typeExpr e s) of
                 (MonadTp et) \<Rightarrow> if (isCorrectTp et) then 
                                    (MflatMap (cpsTreeToExpr (CpsTreeAsync e k1) s) 
                                              (Lambda 1 et (Ref 1 et)))
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
  by pat_completeness auto
termination
  apply( relation "measure (\<lambda> x. case x of (tree, tps) \<Rightarrow> (complexityCpsTree tree))" )
  apply( auto )
  done


 

fun unpureCpsTree :: "CpsTree \<Rightarrow> TpVarState \<Rightarrow> (Expr option)" where
   "unpureCpsTree (CpsTreePure e) s =  (Some e)"
  | 
   "unpureCpsTree (CpsTreeFlatMap source i fbody) s = (
       case (unpureCpsTree source s) of 
         None \<Rightarrow> None
        |
         Some(unpureSource) \<Rightarrow>
           (case (unpureCpsTree fbody s) of
             None \<Rightarrow> None
            |
             Some unpureFbody \<Rightarrow> (
                case typeCpsTree source s of
                   MonadTp tp \<Rightarrow> Some (Let i tp (unpureSource) (unpureFbody) )
             )
           )
   )"
  |
   "unpureCpsTree (CpsTreeAsync e k) s = None"
  |
   "unpureCpsTree (CpsTreeLambda i tpi cpsBody) s = (
      case (unpureCpsTree cpsBody s) of
         Some body \<Rightarrow> Some (Lambda i tpi body)
        |
         None \<Rightarrow> None
   )"
 |
  "unpureCpsTree (CpsTreeLet v vt vv cpsBody) s = (
      case (unpureCpsTree cpsBody s) of
        Some body \<Rightarrow> Some (Let v vt (vv) (body))
        |
        None \<Rightarrow> None 
  )"
 |
  "unpureCpsTree ErrorCpsTree s = None" 


fun asyncKindRepr :: "CpsTree \<Rightarrow> TpVarState \<Rightarrow> AsyncKindRepr" where
  "asyncKindRepr (CpsTreePure expr) tps = AKRPure expr"
 |
  "asyncKindRepr (CpsTreeFlatMap s v b) tps = (
     case (asyncKind (CpsTreeFlatMap s v b)) of
        AKAsync ak \<Rightarrow> AKRAsync ak (CpsTreeFlatMap s v b)
      | AKSync \<Rightarrow> AKRError
      | AKLambda ik \<Rightarrow> AKRError
      | AKError \<Rightarrow> AKRError
  )"
|
  "asyncKindRepr (CpsTreeAsync expr ik) tps = AKRAsync ik (CpsTreeAsync expr ik)" 
|
  "asyncKindRepr (CpsTreeLambda i tp body) tps =(
      case (asyncKind body) of
        AKSync \<Rightarrow> (case (unpureCpsTree body tps) of
                      Some sbody \<Rightarrow> AKRPure (Lambda i tp sbody)
                    | None \<Rightarrow> AKRError
                  )
       |
        AKAsync ik \<Rightarrow> AKRError 
       |
        AKLambda bk \<Rightarrow> AKRLambda bk (CpsTreeLambda i tp body)
       |
        AKError \<Rightarrow> AKRError
     )"
 |
  "asyncKindRepr (CpsTreeLet v vt vv body) tps = (
     case (asyncKindRepr body (tps(v:=vt))) of
      AKRPure be \<Rightarrow> AKRPure (Let v vt vv be)
     | AKRAsync ak bCps \<Rightarrow> AKRAsync ak (CpsTreeLet v vt vv bCps)
     | AKRLambda bk bCps \<Rightarrow> AKRLambda bk (CpsTreeLet v vt vv body)
     | AKRError \<Rightarrow> AKRError
  )"
 |
  "asyncKindRepr ErrorCpsTree tps = AKRError" 






(*
 CpsTransform 
*)
fun exprToCpsTree :: "Expr \<Rightarrow> TpVarState \<Rightarrow> CpsTree" where
    "exprToCpsTree (ConstantExpr c) s =  CpsTreePure (ConstantExpr c)"
  |
    "exprToCpsTree (Ref i tp) s = CpsTreePure (Ref i tp)"
  |
    "exprToCpsTree (Let v vt vv body) s = (
        let cpsv = (exprToCpsTree vv s);
            cpsbody = (exprToCpsTree body s) in
           (case (asyncKindRepr cpsv s) of
             AKRPure pureV \<Rightarrow>
               (case (asyncKindRepr cpsbody (s(v:=vt))) of
                   AKRPure pureBody \<Rightarrow> (CpsTreePure (Let v vt pureV pureBody)) 
                  |AKRAsync ik cpsbody1 \<Rightarrow> CpsTreeLet v vt pureV cpsbody
                  |AKRLambda bk cpsbody \<Rightarrow> CpsTreeLet v vt pureV cpsbody
                  |AKRError \<Rightarrow> errorCpsTree ''translation failed'' 
               ) 
             |AKRAsync vk cpsv \<Rightarrow>
               CpsTreeFlatMap cpsv v cpsbody
             |AKRLambda bk bodyb \<Rightarrow>
                 errorCpsTree ''functional variables are not supported yet''
           )
        
    )"
  |
   "exprToCpsTree (If c e1 e2) s = (
    let cpsC = (exprToCpsTree c s); 
        cpsE1 = (exprToCpsTree e1 s);
        cpsE2 = (exprToCpsTree e2 s) in (
      errorCpsTree ''TODO''  
   ))"
  
    
    

end
