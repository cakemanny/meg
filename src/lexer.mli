
exception SyntaxError of string

(** Scan a single token from the lexbuf *)
val token: Lexing.lexbuf -> Parser.token

(** Set the filename being scanned for the lexer to produce nice error messages *)
val set_filename : string -> Lexing.lexbuf -> Lexing.lexbuf

type class_token =
  | RangeTok of char * char
  | CharTok of char
  | EofTok

val class_token : Lexing.lexbuf -> class_token

