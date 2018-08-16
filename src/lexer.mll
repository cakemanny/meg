{

open Parser

let sprintf = Printf.sprintf

exception SyntaxError of string

let position lexbuf =
  let open Lexing in
  let p = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let set_filename (fname:string) (lexbuf) =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

let error lexbuf fmt =
  Printf.kprintf (fun msg ->
      raise (SyntaxError ((position lexbuf)^" "^msg))) fmt

type class_token =
  | RangeTok of char * char
  | CharTok of char
  | EofTok

let unescape_char s =
  if String.length s > 1 && s.[0] = '\\' then
    match s.[1] with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
      -> s
         |> Stringext.to_list
         |> List.tl
         |> List.fold_left (fun oct c -> (oct * 8) + Char.(code c - code '0')) 0
         |> Char.chr
    | 'b' -> '\b'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | c -> c
  else s.[0]
}

let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_cont  = ident_start | ['0'-'9']
let identifier  = ident_start ident_cont*

(* We've adapated char a bit to work only with range.
 * Notice it won't match [ or ] *)
let char        = '\\' ['b' 'n' 'r' 't' '\'' '"' '[' ']' '\\']
                | '\\' ['0'-'3']['0'-'7']['0'-'7']
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
  | "%}"              {raise @@ error lexbuf "Unmatched '%%}'"}
  | "%%" _* as trail  {TRAILER (String.sub trail 2 @@ (String.length trail - 2))}
  | '{' { BRACES (Buffer.contents @@ read_braces 0 (Buffer.create 32) lexbuf) }
  | '}'  {error lexbuf "Unmatched '}'"}

  | identifier as i   {IDENT i}
  | '"'  {read_literal_dbl (Buffer.create 17) lexbuf}
  | '\'' {read_literal_sgl (Buffer.create 17) lexbuf}
  | clazz as lxm { for i = 0 to (String.length lxm) - 1 do
                     if lxm.[i] = '\n' then Lexing.new_line lexbuf
                   done;
                   CLASS (String.sub lxm 1 @@ String.length lxm - 2 )}
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
  | _  as c {error lexbuf "Illegal character: %c" c }

and read_literal_dbl buf =
  parse
  | '"'       { LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_literal_dbl buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_literal_dbl buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_literal_dbl buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_literal_dbl buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_literal_dbl buf lexbuf }
  | '\n'      { Buffer.add_char buf '\n'; Lexing.new_line lexbuf; read_literal_dbl buf lexbuf }
  | [^ '"' '\\' '\n']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_literal_dbl buf lexbuf
    }
  | eof { error lexbuf "Literal is not terminated" }
  | _ as c { error lexbuf "Illegal character in literal: %c" c }

and read_literal_sgl buf =
  parse
  | '\''      { LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_literal_sgl buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_literal_sgl buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_literal_sgl buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_literal_sgl buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_literal_sgl buf lexbuf }
  | '\n'      { Buffer.add_char buf '\n'; Lexing.new_line lexbuf; read_literal_sgl buf lexbuf }
  | [^ '\'' '\\' '\n']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_literal_sgl buf lexbuf
    }
  | eof { error lexbuf "Literal is not terminated" }
  | _ as c { error lexbuf "Illegal character in literal: %c" c }

and read_declaration buf =
  parse
  | "%}" { DECLARATION (Buffer.contents buf) }
  | '\n' { Buffer.add_char buf '\n'; Lexing.new_line lexbuf; read_declaration buf lexbuf }
  (* More efficient to add a big string while we are guaranteeds no end *)
  | [^ '\n' '%']+ as s { Buffer.add_string buf s; read_declaration buf lexbuf }
  | _ as c { Buffer.add_char buf c; read_declaration buf lexbuf  }
  | eof { error lexbuf "Declaration section not terminated" }

and read_braces level buf =
  parse
| '}' { if level = 0 then buf else (Buffer.add_char buf '}'; read_braces (level - 1) buf lexbuf) }
| '{' { Buffer.add_char buf '}'; read_braces (level + 1) buf lexbuf }
| '\n'{ Buffer.add_char buf '\n'; Lexing.new_line lexbuf; read_braces level buf lexbuf }
| _ as c { Buffer.add_char buf c; read_braces level buf lexbuf }
| eof { error lexbuf "Action not terminated" }

(* *)
and class_token =
  parse
| (char as c1) '-' (char as c2) { RangeTok (unescape_char c1, unescape_char c2) }
| char as c { CharTok (unescape_char c) }
| eof { EofTok }

{


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
