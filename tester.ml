
let atom = Pred("id",[Var "X";Var "X"]);;

let myprog = [Fact atom];;

let prog_goal = Pred("id",[Var "Y"; Node ("Z",[Const "e"])]);;

let tmp = top myprog prog_goal;;   

let fact1 = Fact(Pred("edge",([Const "a"; Const "b"])));;
let fact2 = Fact(Pred("edge",([Const "b"; Const "c"])));;
let fact3 = Fact(Pred("path",([Var "#X"; Var "#X"])));;
let fact4 = Fact(Pred("edge",([Const "c"; Const "d"])));;
let fact5 = Fact(Pred("edge",([Const "d"; Const "e"])));;

let rule1 = Rule(Pred("path",([Var "#X";Var "#Y"])),[Pred("edge",([Var "#X";Var "#Z"]));Pred("path",([Var "#Z";Var "#Y"]))]);;

let myprog2 = [fact2;fact1;fact3;fact4;fact5;rule1];;
let prog_goal = Pred("path",([Const "a"; Var "Y"]));;
let tmp = top myprog2 prog_goal;;


let fact1 = Fact(Pred("male",([Const "a"])));;
let fact2 = Fact(Pred("male",([Const "b"])));;
let fact3 = Fact(Pred("male",([Const "c"])));;
let fact4 = Fact(Pred("female",([Const "d"])));;
let fact5 = Fact(Pred("married",([Const "a";Const "d"])));;
let fact6 = Fact(Pred("married",([Const "d";Const "a"])));;
let fact7 = Fact(Pred("son",([Const "c";Const "a"])));;
let fact8 = Fact(Pred("son",([Const "b";Const "c"])));;
let rule1 = Rule(Pred("son",([Var "#X";Var "#Y"])), ([Pred("married",([Var "#Y";Var "#Z"]));Pred("son",([Var "#X";Var "#Z"]))]));;
let rule3 = Rule(Pred("gson",([Var "#X";Var "#Y"])), ([Pred("son",([Var "#X";Var "#Z"]));Pred("son",([Var "#Z";Var "#Y"]))]));;

let myprog = [fact1;fact2;fact3;fact4;fact6;fact7;fact8;rule1;rule3];;
let prog_goal = Pred("son",([Var "X";Var "Y"]));;
top myprog prog_goal;;
