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



datatype expr = ConstantExpr int
  |
   Let int typeexpr expr expr
  |
   Ref int typeexpr
  |
   Block "expr list" 
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
   ExternalFun int typeexpr
 |
   Error string



fun isError :: "expr \<Rightarrow> bool" where
   "isError (Error s) = True" |
   "isError other = False"

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
    "typeExpr (Block l) s = (case l of
                               [] \<Rightarrow> ConstTp
                              |
                               x # [] \<Rightarrow>  (typeExpr x s)
                              |
                               h # t \<Rightarrow> (typeExpr (Block t) s)
                            )"
  |
    "typeExpr (If cond ifTrue ifFalse) s = (lub (typeExpr ifTrue s) (typeExpr ifFalse s))"
  |
    "typeExpr (While e1 e2) s = ConstTp"
  |
    "typeExpr (Lambda  i tp body) s = (ArrowTp tp (typeExpr body (s(i:=tp))))"
  |
    "typeExpr (App x arg) s =
                (case (typeExpr x s) of
                     (ArrowTp xi xo) \<Rightarrow> if ((typeExpr arg s) = xi) then xo else ErrorTp
                    |
                     other \<Rightarrow> ErrorTp
                )  
    "
  |
    "typeExpr (Mpure x) s = MonadTp (typeExpr x s)"
  |
    "typeExpr (MflatMap fa f) s = (
        case (typeExpr fa s) of
            MonadTp a \<Rightarrow> (case (typeExpr f s) of
                            ArrowTp a1 a2 \<Rightarrow> ConstTp
                          | other \<Rightarrow> ErrorTp)
          | other \<Rightarrow> ErrorTp
    )"
  |
    "typeExpr (ExternalFun fi ft) s = ft"
  | 
    "typeExpr (Error e) s = ErrorTp"

datatype AsyncKind = AKSync | AKAsync AsyncKind | AKLambda AsyncKind | AKError

datatype CpsTree =
        CpsTreePure expr
        |
        CpsTreeFlatMap CpsTree int typeexpr CpsTree
        |
        CpsTreeAsync expr AsyncKind
        |
        CpsTreeLambda int typeexpr CpsTree
        |
        ErrorCpsTree


fun asyncKind :: "CpsTree \<Rightarrow> AsyncKind" where
    "asyncKind (CpsTreePure e) = AKSync"
  |
    "asyncKind (CpsTreeAsync e k) = k"
  |
    "asyncKind (CpsTreeFlatMap source i tp fbody) = (
       case (asyncKind source) of
           (AKAsync Sync) \<Rightarrow> (asyncKind fbody)
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
    "asyncKind ErrorCpsTree = AKError"
      
(*
 analog of CpsTree.transformed
*)
fun cpsTreeToExpr :: "CpsTree \<Rightarrow> expr" where 
   "cpsTreeToExpr (CpsTreePure e) = Mpure e"
 |
   "cpsTreeToExpr (CpsTreeFlatMap arg v vt vbody) = 
                                MflatMap (cpsTreeToExpr arg) (Lambda v vt (cpsTreeToExpr vbody))"
 |
   "cpsTreeToExpr (CpsTreeAsync e k) = (
       case k of
           AKSync \<Rightarrow> e
         | other \<Rightarrow> Error ''Invalid async kind''
   )"   (* TODO: handle AsynK*(Sync)  *)
 |
   "cpsTreeToExpr (CpsTreeLambda v vt vbody) = (Lambda v vt (cpsTreeToExpr vbody))"
  |
   "cpsTreeToExpr ErrorCpsTree = Error ''ErrorCpsTree'' "


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
  "unpureCpsTree ErrorCpsTree = None" 

(*
 CpsTransform 
*)
fun exprToCpsTree :: "expr \<Rightarrow> CpsTree" where
    "exprToCpsTree (ConstantExpr c) =  CpsTreePure (ConstantExpr c)"
  |
    "exprToCpsTree (Let v vt vv body) = (
        let cpsv = exprToCpsTree vv in
         let cpsbody = exprToCpsTree body in
           case (asyncKind cpsv) of 
              AKSync \<Rightarrow>
                case (asyncKind cpsbody) of
                   AKSync \<Rightarrow>
                       CpsTreePure (Let v vt (unpureCpsTree vv) (unpureCpsTree cpsbody))
    )"
    
    


end
