{
open Parser
}

let digit = ['0'-'9']
let id = ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']*

rule read = parse
  | [' ' '\t' '\r' '\n'] { read lexbuf }
  | "//" [^ '\n']* '\n' { read lexbuf }
  | "/*"              { comment lexbuf }
  | "int"             { INT }
  | "void"            { VOID }
  | "if"              { IF }
  | "else"            { ELSE }
  | "while"           { WHILE }
  | "break"           { BREAK }
  | "continue"        { CONTINUE }
  | "return"          { RETURN }
  | "||"              { OR }
  | "&&"              { AND }
  | "=="              { EQ }
  | "!="              { NEQ }
  | "<="              { LE }
  | ">="              { GE }
  | '<'               { LT }
  | '>'               { GT }
  | '='               { ASSIGN }
  | ';'               { SEMI }
  | ','               { COMMA }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '!'               { NOT }
  | '%'               { MOD }
  | id as s           { ID s }
  | digit+ as n  { INT_LIT (int_of_string n) }
  | eof               { EOF }
  | _                 { failwith "Unknown token" }

and comment = parse
  | "*/" {
      read lexbuf
    }
  | "/*" { comment lexbuf }
  | [^ '*''/']+ { comment lexbuf }
  | '*' { comment lexbuf }
  | '/' { comment lexbuf }
  | eof { failwith "Unterminated comment" }