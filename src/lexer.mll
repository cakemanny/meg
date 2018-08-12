{

open Parser
module Lexing

exception SyntaxError of string

}

let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_cont  = ident_start | ['0'-'9']
let identifier  = ident_start ident_cont*

let char        = '\\' ['n' 'r' 't' '\'' '"' '[' ']' '\\']
                | '\\' ['0'-'2']['0'-'7']['0'-'7']
                | '\\' ['0'-'7']['0'-'7']?
                | [^ '\\']
let range       = char '-' char | char
let literal     = '\'' ([^ '\''] | char)* '\''
                | '"' ([^ '"'] | char)* '"'
let clazz       = '[' ([^ ']'] | range)* ']'
let comment     = '#' [^ '\n']* '\n'
(*
;;
*)

rule token = parse
| "%{"              {read_declaration (Buffer.create 256) lexbuf}
| "%%" _* as trail  {TRAILER trail}

| identifier as i   {IDENT i}
| '"'  {read_literal_dbl (Buffer.create 17) lexbuf}
| '\'' {read_literal_sgl (Buffer.create 17) lexbuf}
| clazz as c {CLASS c}
| '='  {EQUAL}
| ':'  {COLON}
| ';'  {SEMI}
| '|'  {BAR}
| '&'  {AMP}
| '!'  {NOT}
| '?'  {QUESTION}
| '*'  {STAR}
| '+'  {PLUS}
| '('  {LPAREN}
| ')'  {RPAREN}
| '.'  {DOT}
| '<'  {LT}
| '>'  {GT}
| '{'  {LBRACE}
| '}'  {RBRACE}
| comment {Lexing.new_line lexbuf; token lexbuf}
| [' ' '\t' '\r'] {tokex lexbuf}
| '\n' {Lexing.new_line lexbuf; token lexbuf}
| eof {EOF}

and read_literal_dbl buf =
  parse
  | '"'       { LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_literal_dbl buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_literal_dbl buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_literal_dbl buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_literal_dbl buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_literal_dbl buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_literal_dbl buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal literal character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Literal is not terminated")) }

and read_literal_sgl buf =
  parse
  | '\''      { LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_literal_sgl buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_literal_sgl buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_literal_sgl buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_literal_sgl buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_literal_sgl buf lexbuf }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_literal_sgl buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal literal character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Literal is not terminated")) }

and read_declaration buf = parse
  | "%}" { DECLARATION (Buffer.contents buf) }
  (* More efficient to add a big string while we are guaranteeds no end *)
  | [^ '%']+ as s { Buffer.add_string buf s; read_declaration buf lexbuf }
  | _ as c { Buffer.add_char buf c; read_declaration buf lexbuf  }
  | eof { raise (SyntaxError ("Declaration section not terminated"))}

{

let () =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.grammar token lexbuf in
  Printf.printf "%s\n" @@ Parser.string_of_expr result

}
