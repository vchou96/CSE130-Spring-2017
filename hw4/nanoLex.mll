{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
  | "let"       { LET }
  | "rec"       { REC }
  | "fun"       { FUN }
  | "in"        { IN }
  | "->"        { ARROW }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "||"        { OR }
  | "&&"        { AND }
  | "="         { EQ }
  | "!="        { NE }
  | "<"         { LT }
  | "<="        { LE }
  | "["         { LBRAC }
  | "]"         { RBRAC }
  | ";"         { SEMI }
  | "::"        { COLONCOLON }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "/"         { DIV }
  | "*"         { MUL }
  | "true"	{ TRUE }
  | "false"	{ FALSE }
  | ' '         { token lexbuf }
  | '\n'        { token lexbuf }
  | '\r'        { token lexbuf }
  | '\t'        { token lexbuf }
  | ['0'-'9']+ as num { Num(int_of_string num) }
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as str { Id(str) }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | eof         { EOF }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
