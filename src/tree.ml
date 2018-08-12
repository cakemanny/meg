
type grammar = section list
and section =
  | Declaration of string
  | Definition of expr
  | Trailer of string
and expr =
  | Rule of string * expr
  | Alternate of expr list
  | Sequence of expr list
  | PeekFor of expr
  | PeekNot of expr
  | Optional of expr
  | Repeat of expr
  | NonEmptyRepeat of expr
  | Name of string
  | ResolvedName of expr (*Rule*)
  | Literal of string
  | Class of string
  | Any

let sprintf = Printf.sprintf

let rec string_of_expr = function
  | Rule (s, e) -> sprintf "%s: %s\n" s @@ string_of_expr e
  | Alternate xs ->
    String.concat "" @@ List.map (fun x -> "| " ^ (string_of_expr x)^ " \n" ) xs
  | Sequence xs -> String.concat " " @@ List.map string_of_expr xs
  | PeekFor e -> sprintf "&(%s)" @@ string_of_expr e
  | PeekNot e -> sprintf "!(%s)" @@ string_of_expr e
  | Optional e -> sprintf "(%s)?" @@ string_of_expr e
  | Repeat e -> sprintf "(%s)*" @@ string_of_expr e
  | NonEmptyRepeat e -> sprintf "(%s)+" @@ string_of_expr e
  | Name n -> n
  | ResolvedName _ -> "<resolved>"
  | Literal s -> sprintf "Lit{{{%s}}}" s
  | Class s -> sprintf "Class[[[%s]]]" s
  | Any -> "."

