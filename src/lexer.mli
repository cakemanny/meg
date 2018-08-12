
exception SyntaxError of string

val token: Lexing.lexbuf -> Parser.token
(** Scan a single token from the lexbuf *)

val set_filename : string -> Lexing.lexbuf -> Lexing.lexbuf
(** Set the filename being scanned for the lexer to produce nice error messages *)

