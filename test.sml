

val _ = (Control.Print.printLength := 1024)
val _ = (Control.Print.printDepth := 20)

datatype expn = LAM of string * expn | APP of expn * expn | VAR of string;

let
	val (x1, t1) = ("zero", LAM("f",LAM("x",VAR "x"))); 
	val (x2, t2) = ("succ", LAM("n",LAM("f",LAM("x",APP(VAR "f",APP(APP(VAR "n",VAR "f"),VAR "x")))))); 
	val t = APP(APP(VAR "succ",VAR "succ"),VAR "zero"); 
	val main = APP(LAM(x1,APP(LAM(x2, t), t2)), t1); 
in
	main
end;