{

open Parser

exception SyntaxError of string

}

let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_cont  = ident_start | ['0'-'9']
let identifier  = ident_start ident_cont*

(* We've adapated char a bit to work only with range.
 * Notice it won't match [ or ] *)
let char        = '\\' ['n' 'r' 't' '\'' '"' '[' ']' '\\']
                | '\\' ['0'-'2']['0'-'7']['0'-'7']
                | '\\' ['0'-'7']['0'-'7']?
                | [^ '\\' '[' ']']
let range       = char '-' char | char
let clazz       = '[' range* ']'

let comment
  = '#' [^ '\n']* '\n'
(*
;;
*)


rule _token = parse
  | "%{"              {read_declaration (Buffer.create 256) lexbuf}
  | "%}"              {raise @@ SyntaxError ("Unmatched '%}'")}
  | "%%" _* as trail  {TRAILER (String.sub trail 2 @@ (String.length trail - 2))}
  | '{' { BRACES (Buffer.contents @@ read_braces 0 (Buffer.create 32) lexbuf) }
  | '}'  {raise @@ SyntaxError ("Unmatched '}'")}

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
  | comment {Lexing.new_line lexbuf; _token lexbuf}
  | [' ' '\t' '\r']+ {_token lexbuf}
  | '\n' {Lexing.new_line lexbuf; _token lexbuf}
  | eof {EOF}
  | _  {raise @@ SyntaxError ("Illegal character: " ^ (Lexing.lexeme lexbuf) )}

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
  | eof { raise (SyntaxError ("Literal is not terminated")) }
  | _ { raise (SyntaxError ("Illegal literal character: " ^ Lexing.lexeme lexbuf)) }

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
  | eof { raise (SyntaxError ("Literal is not terminated")) }
  | _ { raise (SyntaxError ("Illegal literal character: " ^ Lexing.lexeme lexbuf)) }

and read_declaration buf =
  parse
  | "%}" { DECLARATION (Buffer.contents buf) }
  (* More efficient to add a big string while we are guaranteeds no end *)
  | [^ '%']+ as s { Buffer.add_string buf s; read_declaration buf lexbuf }
  | _ as c { Buffer.add_char buf c; read_declaration buf lexbuf  }
  | eof { raise (SyntaxError ("Declaration section not terminated"))}

and read_braces level buf =
  parse
| '}' { if level = 0 then buf else (Buffer.add_char buf '}'; read_braces (level - 1) buf lexbuf) }
| '{' { Buffer.add_char buf '}'; read_braces (level + 1) buf lexbuf }
| _ as c { Buffer.add_char buf c; read_braces level buf lexbuf }
| eof { raise (SyntaxError ("Action not terminated"))}

{

let sprintf = Printf.sprintf

let string_of_tok = function
  | EQUAL -> "EQUAL"
  | COLON -> "COLON"
  | SEMI -> "SEMI"
  | BAR -> "BAR"
  | AMP -> "AMP"
  | NOT -> "NOT"
  | QUESTION -> "QUESTION"
  | STAR -> "STAR"
  | PLUS -> "PLUS"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | DOT -> "DOT"
  | LT -> "LT"
  | GT -> "GT"
  | IDENT s ->  sprintf "IDENT<%s>" s
  | CLASS s ->  sprintf "CLASS<%s>" s
  | LITERAL s ->  sprintf "LITERAL<%s>" s
  | BRACES s -> sprintf "BRACES{%s}" s
  | DECLARATION s -> sprintf "DECLARATION<%s>" s
  | TRAILER s -> sprintf "TRAILER<%s>" s
  | EOF -> "EOF"

let debug = false

let token lexbuf =
  let t = _token lexbuf
  in (
    if debug then Printf.eprintf "%s\n" @@ string_of_tok t;
    t
  )

}
