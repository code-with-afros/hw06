(*run with: rlwrap sml hw06smlTestfunctions.sml*)


(*succ one add1 0  will turn one add1 0 -> two add1 0*)

fun zero f x = x;
fun one f x = f(x);
fun two f x = f(f x);
fun three f x = f(f(f x));

fun succ n f x = f (n f x);

fun pred_helper n nn f x = if (n f x) = (succ nn f x) then (nn f x) else (pred_helper n (succ nn) f x);

fun pred n f x = (pred_helper n zero f x);

fun plus n m = n succ m;
(* (plus two two) add1 4 = 8 *)

(*(minus three one) add1 0 = 2 *)
fun minus n m = m pred n;

fun times n m f x = n (m f) x;

(*(power two three) add1 0 = 8*)
fun power n m = m n;

fun not_true x = false;

fun is_zero n = n not_true true;




fun add1 x = x+1;

fun test_zero f x = x;

fun compare n m f x = if (n f x) = (m f x) then 1 else 0;

(compare zero test_zero add1 1);