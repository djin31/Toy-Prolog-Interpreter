open Parser
open Interpreter

let load_prog filename =
	let fstream = open_in filename in 
	let lexbuf = Lexing.from_channel fstream in
    Parser.database Lexer.lex lexbuf;;

let load_query query = 
	let lexbuf = Lexing.from_string query in
    Parser.query Lexer.lex lexbuf;;


let print_cl cl = match cl with
	|Fact (atom) -> let _ = Printf.printf "Fact " in
					let _ = [print_atom atom] in
					Printf.printf "\n"
	|Rule (atom,body) -> let _ = Printf.printf "Rule " in
						 let _ = print_atom atom in
						 let _ = map print_atom body in
						 Printf.printf "\n";;


let rec main = 
	let _ = Printf.printf "Enter file name\n" in
	let _ = flush stdout in
	let inp = try read_line() with End_of_file -> Printf.printf "\nExiting\n" ;exit 0 in
	let prog = load_prog inp in
	let _ = Printf.printf "Program Loaded, Begin your queries\n" in
	let _ = flush stdout in
	while true do 
		let _ = Printf.printf "?-" in
		let _ = flush stdout in
		let inp = try read_line() with End_of_file -> Printf.printf "\nExiting\n" ;exit 0 in
		try
			let query = load_query inp in
			if (query = []) then 
			Printf.printf "True\n" 
		else
			solve_query prog query 
		with
		| _ -> Printf.printf "Unknown query\n"
		
		
		
	done;;

	

main;;
