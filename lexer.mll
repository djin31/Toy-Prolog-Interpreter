{
	open Parser
}

let cons = (['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*)
let variable = (['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*)
let numeral = (['0'-'9']+)
let comma = ','
let sep = ":-"
let end = '.'
let open_paren = '('
let close_paren = ')'
let open_sq = '['
let close_sq = ']'
let eol = ['\n' '\r']+
let space = [' ' '\t']+
let comment_line ='%'
let sem_col = ';'
let list_diff = '|'
let cut = '!'

rule lex = parse 
|"true" 		{TRUE}	
|"false" 		{FALSE}
|"fail"			{FAIL}
|cons as s 		{CONST(s)}
|variable as v 	{VAR(v)}
|numeral as n   {NUMBER(int_of_string n)}
|'+'			{ADD}
|'-'			{SUB}
|'*'			{MUL}
|'/'			{DIV}
|'*'			{EXP}
|'='			{EQUALS}
|'>'			{GREATER}
|'<'			{LESS}
|comma			{COMMA}
|sep 			{SEP}
|end			{END}
|open_paren		{OPEN_PAREN}
|close_paren	{CLOS_PAREN}
|open_sq		{OPEN_SQ}
|close_sq		{CLOS_SQ}
|eol			{lex lexbuf}
|space 			{lex lexbuf}
|comment_line	{comment lexbuf}
|sem_col		{SEM_COL}
|list_diff		{LIST_DIFF}
|cut 			{CUT}
|eof 			{EOF}

and comment = parse
|eof 			{lex lexbuf}
|eol 			{lex lexbuf}
|_				{comment lexbuf}

