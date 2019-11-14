
val _ = (Control.Print.printLength := 1024)
val _ = (Control.Print.printDepth := 20)

datatype term = 
  VAR of string
| LET of string * term * term
| REC of string * string * term
| LAM of string * term
| APP of term * term 

| UNT

| NUM of int
| NEG of term
| PLUS of term * term
| TIMES of term * term

| LESS of term * term
| EQUAL of term * term

| CND of bool
| IF of term * term * term
| AND of term * term
| OR of term * term
| NOT of term

| PAIR of term * term
| FST of term
| SND of term

| CASE of term * string * term * string * term
| LFT of term
| RGT of term

| NIL
| CONS of term * term
| HD of term
| TL of term
| NULL of term

fun without x nil = nil
  | without x (y::ys) = if x=y then without x ys
                               else y::(without x ys)

fun union nil ys     = ys
  | union (x::xs) ys = x::(union xs (without x ys))

fun FV (VAR x)          = [x]
  | FV (LET (x,t1,t2))  = union (FV t1) (without x (FV t2))
  | FV (LAM (x,t))      = without x (FV t)
  | FV (REC (f,x,t))    = without f (without x (FV t))
  | FV (CASE (t,x1,t1,x2,t2)) = union (FV t) (union (without x1 (FV t1)) (without x2 (FV t2)))
  | FV (APP (t1,t2))    = union (FV t1) (FV t2)
  | FV (PLUS (t1,t2))   = union (FV t1) (FV t2)
  | FV (TIMES (t1,t2))  = union (FV t1) (FV t2)
  | FV (NEG t)          = FV t
  | FV (LESS (t1,t2))   = union (FV t1) (FV t2)
  | FV (EQUAL (t1,t2))  = union (FV t1) (FV t2)
  | FV (IF (t1,t2,t3))  = union (FV t1) (union (FV t2) (FV t3))
  | FV (AND (t1,t2))    = union (FV t1) (FV t2)
  | FV (OR (t1,t2))     = union (FV t1) (FV t2)
  | FV (NOT t)          = FV t
  | FV (PAIR (t1,t2))   = union (FV t1) (FV t2)
  | FV (FST t)          = FV t
  | FV (SND t)          = FV t
  | FV (LFT t)          = FV t
  | FV (RGT t)          = FV t
  | FV (CONS (t1,t2))   = union (FV t1) (FV t2)
  | FV (HD t)           = FV t
  | FV (TL t)           = FV t
  | FV (NULL t)         = FV t
  | FV _                = nil

fun isValue (LAM _)        = true
  | isValue (REC _)        = true
  | isValue (NUM _)        = true
  | isValue (CND _)        = true
  | isValue UNT            = true
  | isValue (PAIR (t1,t2)) = (isValue t1) andalso (isValue t2)
  | isValue t              = isListValue t

and isListValue NIL           = true
  | isListValue (CONS(t1,t2)) = (isValue t1) andalso (isListValue t2)
  | isListValue _             = false

fun subst (x,v) t = case t of
    (VAR y) => 
           if x=y then v 
                  else (VAR y)
  | (LET (y,t1,t2)) => 
           if x=y then LET (y, subst (x,v) t1, t2)
                  else LET (y, subst (x,v) t1, subst (x,v) t2)
  | (LAM (y,r)) =>
           if x=y then t
                  else LAM (y, subst (x,v) r)
  | (REC (f,y,r)) =>
           if x=y orelse x=f
                  then t
                  else (REC (f,y,subst (x,v) r))
  | (APP (t1,t2))   => APP (subst (x,v) t1, subst (x,v) t2)
  | (PLUS (t1,t2))  => PLUS (subst (x,v) t1, subst (x,v) t2)
  | (TIMES (t1,t2)) => TIMES (subst (x,v) t1, subst (x,v) t2)
  | (NEG t)         => NEG (subst (x,v) t)
  | (LESS (t1,t2))  => LESS (subst (x,v) t1, subst (x,v) t2)
  | (EQUAL (t1,t2)) => EQUAL (subst (x,v) t1, subst (x,v) t2)
  | (IF (t1,t2,t3)) => IF (subst (x,v) t1, subst (x,v) t2, subst (x,v) t3)
  | (AND (t1,t2))   => AND (subst (x,v) t1, subst (x,v) t2)
  | (OR (t1,t2))    => OR (subst (x,v) t1, subst (x,v) t2)
  | (NOT t)         => NOT (subst (x,v) t)
  | (NULL t)        => NULL (subst (x,v) t)
  | (HD t)          => HD (subst (x,v) t)
  | (TL t)          => TL (subst (x,v) t)
  | (CONS (t1,t2))  => (CONS (subst (x,v) t1,subst (x,v) t2))
  | (PAIR (t1,t2))  => (PAIR (subst (x,v) t1,subst (x,v) t2))
  | (FST t)          => FST (subst (x,v) t)
  | (SND t)          => SND (subst (x,v) t)
  | _  => t

fun reduceStep t = case t of
    (LET (x,s,t))        => if isValue s 
                            then subst (x,s) t
                            else LET (x,reduceStep s,t)
  | (APP (LAM(x,t),s))   => if isValue s 
                            then subst (x,s) t
                            else (APP (LAM(x,t),reduceStep s))
  | (APP (REC(f,x,t),s)) => if isValue s 
                            then subst (f,REC(f,x,t)) (subst (x,s) t)
                            else (APP (REC(f,x,t),reduceStep s))                    
  | (APP (t1,t2))        => APP (reduceStep t1,t2)

  | (NEG (NUM i)) => NUM (~i)
  | (NEG t)       => NEG (reduceStep t)

  | (PLUS (NUM i1, NUM i2)) => NUM (i1+i2)
  | (PLUS (NUM i1, t2))     => PLUS (NUM i1, reduceStep t2)
  | (PLUS (t1,t2))          => PLUS (reduceStep t1,t2)

  | (TIMES (NUM i1, NUM i2)) => NUM (i1*i2)
  | (TIMES (NUM i1, t2))     => TIMES (NUM i1, reduceStep t2)
  | (TIMES (t1,t2))          => TIMES (reduceStep t1,t2)

  | (LESS (NUM i1, NUM i2)) => CND (i1<i2)
  | (LESS (NUM i1, t2))     => LESS (NUM i1, reduceStep t2)
  | (LESS (t1,t2))          => LESS (reduceStep t1,t2)

  | (EQUAL (NUM i1, NUM i2))          => CND (i1=i2)
  | (EQUAL (CND b1, CND b2))          => CND (b1=b2)
  | (EQUAL (UNT, UNT))                => CND true
  | (EQUAL (PAIR(l1,r1),PAIR(l2,r2))) => CND((l1=l2) andalso (r1=r2))
  | (EQUAL (t1,t2)) => if isValue t1 
                       then EQUAL (t1, reduceStep t2)
                       else EQUAL (reduceStep t1, t2)              
  
  | (IF (CND b,t1,t2)) => if b then t1 else t2
  | (IF (t1,t2,t3))    => IF (reduceStep t1,t2,t3)

  | (AND (CND false,_))    => CND false
  | (AND (CND true,CND b)) => CND b
  | (AND (CND true,t2))    => AND (CND true,reduceStep t2)

  | (OR (CND true,_))      => CND true
  | (OR (CND false,CND b)) => CND b
  | (OR (CND false,t2))    => OR (CND false,reduceStep t2)

  | (NOT (CND b)) => CND (not b)
  | (NOT t)       => NOT (reduceStep t)

  | (NULL NIL)    => CND true
  | (NULL t)      => if isListValue t
    	             then CND false
                     else (NULL (reduceStep t)) 
 
  | (CONS (t1,t2)) => if isValue t1 
                      then CONS (t1, reduceStep t2)
                      else CONS (reduceStep t1, t2)
  | (HD (t as (CONS (t1,t2)))) => 
                      if isValue t 
                      then t1
                      else if isValue t1 
                           then (HD (CONS (t1,reduceStep t2)))
                           else (HD (CONS (reduceStep t1,t2)))
  | (HD t) => HD (reduceStep t)
  | (TL (t as (CONS (t1,t2)))) => 
                      if isValue t 
                      then t2
                      else if isValue t1 
                           then (TL (CONS (t1,reduceStep t2)))
                           else (TL (CONS (reduceStep t1,t2)))
  | (TL t) => TL (reduceStep t)

  | (PAIR (t1,t2)) => if isValue t1 
                      then PAIR (t1, reduceStep t2)
                      else PAIR (reduceStep t1, t2)
  | (FST (t as (PAIR (t1,t2)))) => 
                      if isValue t 
                      then t1
                      else if isValue t1 
                           then (FST (PAIR (t1,reduceStep t2)))
                           else (FST (PAIR (reduceStep t1,t2)))
  | (FST t) => FST (reduceStep t)
  | (SND (t as (PAIR (t1,t2)))) => 
                      if isValue t 
                      then t2
                      else if isValue t1 
                           then (SND (PAIR (t1,reduceStep t2)))
                           else (SND (PAIR (reduceStep t1,t2)))
  | (SND t) => SND (reduceStep t)

  | _  => t

fun reducesTo t = let val t' = reduceStep t
                  in if t=t' then t' else reducesTo t'
                  end;

(*
val map_t = REC("map","f",
           LAM("xs",
               IF (NULL (VAR "xs"),
                   NIL,
                   LET("h",HD (VAR "xs"),
                   LET("t",TL (VAR "xs"),
                   LET("fh",APP (VAR "f",VAR "h"),
                   LET("ft",APP (APP (VAR "map",VAR "f"),VAR "t"), 
                   CONS (VAR "fh",VAR "ft"))))))))
val range_t = REC("range","n",
           LAM("m",
               CONS (VAR "n",IF (EQUAL (VAR "n",VAR "m"),
                                 NIL,
                                 APP (APP (VAR "range", PLUS (VAR "n",NUM 1)), VAR "m")))))
*)
val sqr_t = LAM("x",TIMES(VAR "x",VAR "x"));
(*
val r1to5_t= APP(APP(range_t,NUM 1),NUM 5)
val test0 = APP(APP(map_t,sqr_t),r1to5_t)

val revhelp_t = REC("rh","xs_rxs",
                 IF(NULL(FST(VAR "xs_rxs")),
                   SND (VAR "xs_rxs"),
                   LET ("xs",FST(VAR "xs_rxs"),
                   LET ("rxs",SND(VAR "xs_rxs"),
                   APP(VAR "rh",PAIR(TL(VAR "xs"),CONS(HD(VAR "xs"),VAR "rxs")))))))
val reverse_t = LAM("xs",APP(VAR "revhelp",PAIR(VAR "xs",NIL)))
val test1 = LET("revhelp",revhelp_t,
            LET("reverse",reverse_t,
	    APP(VAR "reverse",r1to5_t)))

val fiblist_t = REC("fl","n",
                 IF(EQUAL(VAR "n",NUM 0),
                    CONS(NUM 0,NIL),
                    IF(EQUAL(VAR "n",NUM 1),
                       CONS(NUM 1,CONS(NUM 0,NIL)),
                       LET("fibs",APP(VAR "fl",PLUS(VAR "n",NEG(NUM(1)))),
                       CONS(PLUS(HD(VAR "fibs"),HD(TL(VAR "fibs"))),VAR "fibs")))))

val test2 = LET("fiblist",fiblist_t,
            APP(VAR "fiblist",NUM 10))
val test3 = LET("revhelp",revhelp_t,
            LET("reverse",reverse_t,
            LET("fiblist",fiblist_t,
            APP(VAR "reverse",APP(VAR "fiblist",NUM 10)))))
*)

 


