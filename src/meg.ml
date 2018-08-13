(* module T = Tree *)


let parse filename (action : Tree.grammar -> unit)  =
  let channel = open_in filename in
  try
    let lexbuf = Lexing.from_channel channel in
    let lexbuf = Lexer.set_filename filename lexbuf in
    try
      let result = Parser.grammar Lexer.token lexbuf in
      action result
    with
    | Parser.Error -> Printf.eprintf "It's all gone wrong\n"
    | Lexer.SyntaxError msg -> Printf.eprintf "%s\n" msg
  with e ->
    close_in_noerr channel;
    raise e

let print_result result =
    List.iter (
      function
      | Tree.Declaration s -> Printf.printf "%%{%s%%}" s
      | Tree.Definition e -> Printf.printf "%s\n" @@ Tree.string_of_expr e
      | Tree.Trailer s -> (print_endline "%%" ; print_endline s)
    ) result

let default default_value = function Some value -> value | None -> default_value

let print_actions rules =
  let open Tree in
  let rec walk rule_name action_id = function
    | Rule (name, expr)
      -> walk (Some name) action_id expr
    | Alternate es
    | Sequence es
      -> List.fold_left (walk rule_name) action_id es
    | PeekFor expr
    | PeekNot expr
    | Optional expr
    | Repeat expr
    | NonEmptyRepeat expr
    | Capture expr
      -> walk rule_name action_id expr
    | Name (_, _)
    | Literal _
    | Class _
    | Any
      -> action_id
    | Action text
    | Predicate text
      -> (
          (* TODO: Add params and define variables   *)
          Printf.printf "let yy_%d_%s () = (\n%s\n);;\n"
            (action_id + 1) (default "" rule_name) text;
          action_id + 1
        )
  in List.fold_left (walk None) 0 rules

let compile_rules (rules : Tree.expr list) =
  (* TODO: check for left recursion *)
  (* TODO: create a rule lookup *)
  let _ = print_actions rules in
  ()

let compile_result result =
  let open Tree in
  (* Print headers *)
  List.iter(
    function
    | Declaration declaration -> Printf.printf "%s" declaration
    | Definition _ -> ()
    | Trailer _ -> ()
  ) result;
  (* compile nodes *)
  let rules = List.fold_right ( (* filter_map *)
      fun section xs -> match section with
        | Definition rule -> rule :: xs
        | _ -> xs
    ) result []
  in compile_rules rules;
  (* dump trailer *)
  List.iter(
    function
    | Declaration _ -> ()
    | Definition _ -> ()
    | Trailer trailer -> Printf.printf "%s" trailer
  ) result


let () =
  match (Array.to_list Sys.argv) with
  | [] -> Printf.eprintf("unreachable")
  | progname :: [] -> Printf.eprintf "usage: %s <filename>\n" progname
  | progname :: filename :: _ -> begin
      parse filename compile_result
    end
