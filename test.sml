use "norReduce.sml";

let
	val (x1, t1) = ("zero", LAM("f",LAM("x",VAR "x"))) 
	val (x2, t2) = ("one", APP(VAR "succ",VAR "zero")) 
	val (x3, t3) = ("pred", LAM("n",LAM("f",LAM("x",APP(APP(APP(VAR "n",LAM("g",LAM("h",APP(VAR "h",APP(VAR "g",VAR "f"))))),LAM("u",VAR "x")),LAM("u",VAR "u")))))) 
	val t = APP(VAR "pred",VAR "one") 
	val main = APP(LAM(x1,APP(LAM(x2,APP(LAM(x3, t), t3)), t2)), t1) 
in
	reducesTo main
end;