
type error =
    Unclosed of Lexing.position * string * Lexing.position * string
  | Expecting of Lexing.position * string
  | NotExpecting of Lexing.position * string
  | General of Lexing.position * string

exception Error of error

let sprintf = Printf.sprintf

let pp_loc p =
  Lexing.(sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol))

let pp_err =
  function
  | Unclosed (start_pos, start_name, _, _) ->
    sprintf "%s possibly unclosed \"%s\"" (pp_loc start_pos) start_name
  | Expecting (pos, name) ->
    sprintf "%s expecting \"%s\"" (pp_loc pos) name
  | NotExpecting (pos, name) ->
    sprintf "%s not expecting \"%s\"" (pp_loc pos) name
  | General (pos, msg) ->
    sprintf "%s %s" (pp_loc pos) msg

