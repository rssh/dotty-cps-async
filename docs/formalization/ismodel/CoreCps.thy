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
   Let int typeexpr expr
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

fun lastExpression::(Block


fun typeExpr :: "expr \<Rightarrow> typeVarState \<Rightarrow> typeexpr" where
    "typeExpr (ConstantExpr x) s = ConstTp"
  |
    "typeExpr (Let v vt body) s = (typeExpr body (s(v:=vt)) )"
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
    "typeExpr (Error e) s = ErrorTp"



end
