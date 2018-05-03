exception NOT_UNIFIABLE;;
exception FAILURE;;

type term = 
	|Var of string
	|Const of string
	|Nat of int
	|T
	|F
	|Node of string*(term list)
	|CutCookie;;

type atomic = Pred of string*(term list)|Cut|Fail;;

type clause =  
	|Fact of atomic
	|Rule of atomic*(atomic list);;

type program = clause list;;  

type goal = atomic list;;

let rec map f l = match l with 
	|[] -> []
	|x::xs -> (f x)::(map f xs);;

let rec fold f e l = match l with 
	|[] -> e
	|x::xs -> fold f (f x e) xs;;

let rec reduce f l e = match l with
      |[] -> e
      |(x::xs) -> reduce f xs (f e x);;

let concat l1 l2 = l1@l2;;

let rec filter f l = match l with
      |[] -> []
      |(x::xs) -> if (f x) then (x::(filter f xs)) else (filter f xs);;

(* returns compliment of the result given by the filter*)
let rec neg_filter f l = match l with
	|[] -> []
	|(x::xs) -> if (f x) then (filter f xs) else (x::(filter f xs));;

(*checks if element exists in list*)
let rec find l e = match l with
	|[] -> false
	|x::xs -> if x = e then true else (find xs e);;  

let rec union l1 l2 = match l2 with
	|[] -> l1
	|x::xs -> if (find l1 x) then (union l1 xs) else (union (x::l1) xs);;

(* find if a pair with given first element exists *)
let rec find_pair pairlist v = match pairlist with 
	|[] -> false
	|(vx,t)::xs -> if (vx = v) then true else (find_pair xs v);;   

(* to return union based on keys of two list containing key value pair *)
let rec union_pair l1 l2 = match l2 with
	|[] -> l1
	|(v,t)::xs -> if (find_pair l1 v) then (union l1 xs) else (union ((v,t)::l1) xs);;

(* return substituent value for a variable *)
let rec findsub s v = match s with
	|[] -> v
	|(vx , t)::xs -> if (vx = v) then t else (findsub xs v);;   

(* implements homomorphic extension of substitution *)
let rec subst sigma t = match t with
	|Var v ->  (findsub sigma t)
	|Node(s,l) -> Node(s,(map (subst sigma) l))
	|_ -> t;; 

(* composes a particular substitution pair with another substitution *)
let rec give_sub sigma sub = match sub with
	|(t1,t2) -> (t1,subst sigma t2);;

(* composes two substitutions *)
let compose sig1 sig2 = union_pair (map (give_sub sig2) sig1) sig2;; 

(* implements fold operation on l1 l2 with a function which accepts two arguments *)
let rec unify s f l1 l2 = match (l1,l2) with
	|([],[]) -> s
	|(x1::xs1,x2::xs2) -> let s' = (compose s (f x1 x2)) in (unify s' f (map (subst s') xs1) (map (subst s') xs2))
	|([],(x::xs)) -> raise NOT_UNIFIABLE
	|((x::xs),[]) -> raise NOT_UNIFIABLE;;
      
(*finds most general unifier*)
let rec mgu t1 t2 = match (t1,t2) with
	|(Var v1, Const n2) -> [t1,t2]
	|(Const n1, Const n2) -> if (n1=n2) then [] else raise NOT_UNIFIABLE
	|(Const n1, Var v2) -> [t2,t1]
	|(Var v1, Nat n2) -> [t1,t2]
	|(Nat n1, Nat n2) -> if (n1=n2) then [] else raise NOT_UNIFIABLE
	|(Nat n1, Var v2) -> [t2,t1]
	|(Var v1, T) -> [t1,t2]
	|(T, Var v2) -> [t2,t1]
	|(T, T) -> []
	|(Var v1, F) -> [t1,t2]
	|(F, Var v2) -> [t2,t1]
	|(F, F) -> []
	|(Var v1, Var v2) -> if (v1=v2) then [] else [t1,t2]
	|(Var v1, Node(s,l)) -> if (find l t1) then raise NOT_UNIFIABLE else [t1,t2]
	|(Node(s,l), Var v2) -> if (find l t2) then raise NOT_UNIFIABLE else [t2,t1]
	|(Node(s1,l1),Node(s2,l2)) -> if (s1 = s2) then (unify [] mgu l1 l2) else raise NOT_UNIFIABLE
	|_ -> raise NOT_UNIFIABLE;;

let rec subst_atomic sigma atm = match atm with
	|(Pred(sym,l)) -> (Pred(sym,(map (subst sigma) l)))
	|_ -> atm;;

let rec mgu_atomic atm1 atm2 = match (atm1,atm2) with
      |(Pred(sym1,l1),Pred(sym2,l2)) ->
      	if (sym1 = sym2) 
      	then (unify [] mgu l1 l2) 
      	else raise NOT_UNIFIABLE
      |_ -> raise NOT_UNIFIABLE;;

(*finds variables in term*)
let rec find_vars t = match t with
	|Var v -> [t]
	|Node(s,(x::xs)) -> (fold union [] (map find_vars (x::xs)))
	|_ -> [];;

let find_vars_atomic atm = match atm with
      |(Pred(sym,l1)) -> (fold union [] (map find_vars l1))
      |_ -> [];;

let rec findfail l = match l with
	|[] -> false
	|(Fail::xs) -> true
	|(x::xs) -> findfail xs;;

let rec findcut l = match l with 
	|[] -> false
	|([CutCookie,CutCookie]::xs) -> true
	|(x::xs) -> findcut xs;;

let rec removeCut l = 
	let l' = List.rev l in
	List.rev (List.tl l');;

let rec solve_goal prog workingprog goal unifs = match (workingprog) with
	|[] -> unifs
	|((Fact atm1)::workingprog') -> 
	    (try
				let sol = mgu_atomic atm1 goal in
				(solve_goal prog workingprog' goal (unifs@[sol]))

	    with 
				|NOT_UNIFIABLE -> solve_goal prog workingprog' goal unifs)
	|((Rule (atm1,body))::workingprog') -> 
	    try
				if (findfail body) 
				then solve_goal prog workingprog' goal unifs
				else
	                let sol = mgu_atomic atm1 goal in
	                let furthersols = (solve_goallist prog prog (map (subst_atomic sol) body) sol) in
	                if (findcut furthersols)
	                then (unifs@furthersols)
	                else (solve_goal prog workingprog' goal (unifs@furthersols))
	          
	    with
	        	|NOT_UNIFIABLE -> solve_goal prog workingprog' goal unifs

and solve_goallist prog unseenprog newgoals sol_till_now= match (unseenprog,newgoals) with
      |([],g::gt) -> []
      |(_,Cut::gt) -> (solve_goallist prog unseenprog gt sol_till_now)@[[CutCookie,CutCookie]]
      |((Fact(f)::ps),[g]) -> 
            (try
                  let sol = mgu_atomic f g in
                  (sol_till_now@sol)::(solve_goallist prog ps [g] sol_till_now)
            with
            | NOT_UNIFIABLE -> solve_goallist prog ps [g] sol_till_now)
      |((Rule(r,body)::ps),[g]) ->
            (try 
                  if (findfail body) 
                  then solve_goallist prog ps [g] sol_till_now
                  else
                        let sol = mgu_atomic r g in
                        let furthersols = solve_goallist prog prog (map (subst_atomic sol) body) (sol_till_now@sol) in
                        if (findcut furthersols)
                        then (furthersols)
                        else (
                              if (furthersols = [])
                              then (solve_goallist prog ps [g] sol_till_now)
                              else (furthersols@(solve_goallist prog ps [g] sol_till_now))
                        )
            with
            |NOT_UNIFIABLE -> solve_goallist prog ps [g] sol_till_now)
      |((Fact(f)::ps),g::gt) ->
            (try
                  let sol = mgu_atomic f g in
                  (solve_goallist prog prog (map (subst_atomic sol) gt) (sol_till_now@sol))@(solve_goallist prog ps newgoals sol_till_now)
            with
            | NOT_UNIFIABLE -> (solve_goallist prog ps newgoals sol_till_now))
      |((Rule(r,body)::ps),g::gt) ->
            (try 
                  let sol = mgu_atomic r g in
                  let furthersols = solve_goallist prog prog (map (subst_atomic sol) body) (sol_till_now@sol) in
                  if (findcut furthersols)
                  then (reduce concat (map (solve_goallist prog prog gt) furthersols) [])
                  else
                  (reduce concat (map (solve_goallist prog prog gt) furthersols) [])@(solve_goallist prog ps newgoals sol_till_now)
                  
            with
            | NOT_UNIFIABLE -> (solve_goallist prog ps newgoals sol_till_now))
      |_ -> raise FAILURE;;


let rec eval_term term = match term with
      |Node ("Equals",[t1;t2]) -> 
            (match ((eval_term t1),(eval_term t2)) with
                  |(Nat n1,Nat n2) -> if (n1=n2) then T else F
                  | _ -> term)
      |Node ("Greater",[t1;t2]) -> 
            (match ((eval_term t1),(eval_term t2)) with
                  |(Nat n1,Nat n2) -> if (n1>n2) then T else F
                  | _ -> term)
      |Node ("Less",[t1;t2]) -> 
            (match ((eval_term t1),(eval_term t2)) with
                  |(Nat n1,Nat n2) -> if (n1<n2) then T else F
                  | _ -> term)
      |Node ("Add",[t1;t2]) -> 
            (match ((eval_term t1),(eval_term t2)) with
                  |(Nat n1,Nat n2) -> Nat (n1+n2)
                  | _ -> term)
      |Node ("Sub",[t1;t2]) -> 
            (match ((eval_term t1),(eval_term t2)) with
                  |(Nat n1,Nat n2) -> Nat (n1-n2)
                  | _ -> term)
      |Node ("Mul",[t1;t2]) -> 
            (match ((eval_term t1),(eval_term t2)) with
                  |(Nat n1,Nat n2) -> Nat (n1*n2)
                  | _ -> term)
      |Node ("Div",[t1;t2]) -> 
            (match ((eval_term t1),(eval_term t2)) with
                  |(Nat n1,Nat n2) -> Nat (n1/n2)
                  | _ -> term)
      |Node("Cons",[x;xs]) -> 
            (match (eval_term xs) with
                  | Node ("List",l) -> Node ("List", (eval_term x)::l)
                  | _ -> term)
      |Node("Cons",[x]) -> Node ("List", [eval_term x])
      | _ -> term;;

let rec filter_unifier goalvars unif = match unif with
	|[] -> []
	|((v,sub)::xs) -> if (find goalvars v) then ((v,sub)::(filter_unifier goalvars xs)) else (filter_unifier goalvars xs);;


let rec print_terms t = match t with
      |Var v -> Printf.printf " Var %s " v
      |Const s-> Printf.printf " %s " s
      |Nat n -> Printf.printf " %d " n
      |T -> Printf.printf " true "
      |F -> Printf.printf " false "
      |Node (s,l) ->  let _ = Printf.printf " Node (%s, " s in let _ = List.hd (map print_terms (List.rev l)) in Printf.printf ") "
      |CutCookie -> Printf.printf " Cut ";;

let rec print_unifs unifs = match unifs with
      |[] -> Printf.printf "empty unifier"
      |((v,sub)::[]) -> let _ = print_terms v in let _ = Printf.printf " = " in
                        print_terms (eval_term sub)
      |((v,sub)::xs) -> let _ = print_terms v in let _ = Printf.printf " = " in
                        let _ = print_terms (eval_term sub) in let _ = Printf.printf " , " in
                        (* let _ = print_string "\n" in *)
                        print_unifs xs;;

let rec print_atom atm = match atm with
      |(Pred(sym,tl)) -> Printf.printf " %s " sym; (map print_terms tl)
      |Cut -> [Printf.printf " CUT "]
      |Fail -> [Printf.printf " FAIL "];;

let get1char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let rec modify_vars_term t = match t with
      |Var s -> Var("#"^s)
      |Node (s,l) -> Node (s,(map modify_vars_term l))
      |_ -> t;;

let modify_vars_atm atom = match atom with
      |Pred (s,l) -> Pred (s,(map modify_vars_term l))
      |_ -> atom;;

let modify_vars_clause cl = match cl with
      |Fact (atm) -> Fact(modify_vars_atm atm)
      |Rule (head,body) -> Rule(modify_vars_atm head, map modify_vars_atm body);;

let rec curate_prog cprog prog = match prog with 
      |[] -> List.rev cprog
      |(x::xs) -> curate_prog ((modify_vars_clause x)::cprog) xs;;
                
let solve_query prog goal = 
      let goalvars = reduce concat (map find_vars_atomic goal) [] in
      let rprog = curate_prog [] prog in
      let unifs = solve_goallist rprog rprog goal [] in
      let solutions = ref (map (filter_unifier goalvars) unifs) in
      match !solutions with
      |[[]] ->    flush Pervasives.stdout;
                  Printf.printf "\n True\n\n";
                  flush Pervasives.stdout
      |[] ->      flush Pervasives.stdout;
                  Printf.printf "\n False\n\n";
                  flush Pervasives.stdout
      |_ ->
            let choice = ref ';' in
            while (!choice = ';') do
                  try 
                        let unif = List.hd (!solutions) in
                        solutions := List.tl (!solutions);
                        if (unif =[])
                        then choice := '.'
                        else
                              flush Pervasives.stdout;
                              Printf.printf "\n Press ; to continue search\n";
                              print_unifs unif;
                              flush Pervasives.stdout;
                              choice := get1char()
                  with
                  |_ -> flush Pervasives.stdout;
                        Printf.printf "\n False\n\n";
                        flush Pervasives.stdout;
                        choice := '.'
            done;;
