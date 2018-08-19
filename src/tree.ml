
type grammar =
  section list
and section =
  | Declaration of string * Lexing.position * Lexing.position
  | Definition of expr
  | Trailer of string * Lexing.position * Lexing.position
and expr =
  | Rule of string * expr
  | Alternate of expr list
  | Sequence of expr list
  | PeekFor of expr
  | PeekNot of expr
  | Optional of expr
  | Repeat of expr
  | NonEmptyRepeat of expr
  | Capture of expr (* subexpressions of a Capture capture any input matched *)
  | Assign of string * expr
  | Name of string
  | Literal of string
  | Class of string
  | Any
  | Action of string (* Code to execute in the target language *)
  | Predicate of string (* A predicate in the target language *)


let rec string_of_expr =
  let sprintf = Printf.sprintf in
  function
  | Rule (s, e) -> sprintf "%s: %s\n" s @@ string_of_expr e
  | Alternate xs ->
    String.concat "\n  | " @@ List.map string_of_expr xs
  | Sequence xs -> String.concat " " @@ List.map string_of_expr xs
  | PeekFor e -> sprintf "&(%s)" @@ string_of_expr e
  | PeekNot e -> sprintf "!(%s)" @@ string_of_expr e
  | Optional e -> sprintf "(%s)?" @@ string_of_expr e
  | Repeat e -> sprintf "(%s)*" @@ string_of_expr e
  | NonEmptyRepeat e -> sprintf "(%s)+" @@ string_of_expr e
  | Capture e -> sprintf "< %s >" (string_of_expr e)
  | Assign (n, e) -> sprintf "%s=%s" n (string_of_expr e)
  | Name n -> n
  | Literal s -> sprintf "\"%s\"" @@ String.escaped s
  | Class s -> sprintf "[%s]" s (* These have retained their [ ] *)
  | Any -> "."
  | Action s -> sprintf "{ %s }" s
  | Predicate s -> sprintf "&{ %s }" s

