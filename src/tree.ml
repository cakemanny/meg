
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
  | Name of string * string option (* varname *)
  | Literal of string
  | Class of string
  | Any
  | Action of string (* Code to execute in the target language *)
  | Predicate of string (* A predicate in the target language *)
  | Capture of expr (* subexpressions of a Capture capture any input matched *)

let sprintf = Printf.sprintf

let str_to_list s =
  let rec aux i xs =
    if i = 0
    then xs
    else aux (i - 1) (s.[i-1] :: xs)
  in aux (String.length s) []

let list_to_str xs =
  let buf = Buffer.create 32 in
  let () = List.fold_left (fun () c -> Buffer.add_char buf c) () xs
  in Buffer.contents buf

let flat_map f xs = List.flatten @@ List.map f xs

let rec string_of_expr = function
  | Rule (s, e) -> sprintf "%s: %s\n" s @@ string_of_expr e
  | Alternate xs ->
    String.concat "\n  | " @@ List.map string_of_expr xs
  | Sequence xs -> String.concat " " @@ List.map string_of_expr xs
  | PeekFor e -> sprintf "&(%s)" @@ string_of_expr e
  | PeekNot e -> sprintf "!(%s)" @@ string_of_expr e
  | Optional e -> sprintf "(%s)?" @@ string_of_expr e
  | Repeat e -> sprintf "(%s)*" @@ string_of_expr e
  | NonEmptyRepeat e -> sprintf "(%s)+" @@ string_of_expr e
  | Name (n, v) -> (
      match v with
      | Some varname -> sprintf "%s=%s" varname n
      | None -> n
    )
  | Literal s ->
    let escape str =
      if String.contains str '\''
      then list_to_str @@ flat_map (function '\'' -> ['\\';'\''] | c -> [c]) @@ str_to_list str
      else str
    in sprintf "'%s'" @@ escape s
  | Class s -> sprintf "%s" s (* These have retained their [ ] *)
  | Any -> "."
  | Action s -> sprintf "{ %s }" s
  | Predicate s -> sprintf "&{ %s }" s
  | Capture e -> sprintf "< %s >" (string_of_expr e)

