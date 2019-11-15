
val _ = (Control.Print.printLength := 1024)
val _ = (Control.Print.printDepth := 100)


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


fun subst (x,v) t = case t of
    (VAR y) => 
           if x=y then v 
                  else (VAR y)
  | (LAM (y,r)) =>
           if x=y then t
                  else LAM (y, subst (x,v) r)
  | (APP (t1,t2))   => APP (subst (x,v) t1, subst (x,v) t2)
  | _  => t


fun reduceStep t = case t of
   (APP (LAM(x,t),s))   => (subst(x,s) t)
  | (APP (VAR x,t))       => (APP(VAR x, reduceStep t))
  | (APP (APP(x1,t1), VAR x2)) => (APP (reduceStep (APP(x1,t1)), VAR x2) )                    
  | (APP (t1,t2))        => APP (reduceStep t1,t2)

  | (LAM (x,t))    => (LAM (x,reduceStep t))

  | _  => t

fun reducesTo t = let val t' = reduceStep t
                  in if t=t' then t' else reducesTo t'
                  end;

