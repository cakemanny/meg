(* module T = Tree *)

let sprintf = Printf.sprintf


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
;;

type caml_expr =
    CMatchExpr of { matchee : caml_expr;
                    patlist : (caml_expr * caml_expr) list }
  | CName of string
  | CLit of string
  | CCtor of string * caml_expr list
  | CVerb of string (* verbatim ocaml output  *)
  | CTuple of caml_expr list
  | CApp of caml_expr * caml_expr
;;


let rec string_of_caml = function
  | CMatchExpr { matchee; patlist } ->
    let strpat (p,e) =
      ("| " ^ (string_of_caml p) ^ " -> (" ^ (string_of_caml e) ^ ")")
    in
    sprintf "match (%s) with %s"
      (string_of_caml matchee) @@ String.concat "" @@ List.map strpat patlist
  | CName name -> name
  | CLit lit -> sprintf "\"%s\"" lit
  | CCtor (ctor, cexprs) ->
    sprintf "%s (%s)" ctor @@ string_of_caml (CTuple cexprs)
  | CVerb verbatim -> verbatim
  | CTuple cexprs -> String.concat "," @@ List.map string_of_caml cexprs
  | CApp (e1, e2) -> sprintf "(%s) (%s)" (string_of_caml e1) (string_of_caml e2)


let rec compile_node (inputstate,cap) node =
  let open Tree in
  let input_n n = CName ("input" ^ (string_of_int n)) in
  let value_n n = CName ("value" ^ (string_of_int n )) in
  match node with
  | Rule (_,_) -> assert false
  | Alternate es (* chain matches on the error case *)
    -> List.fold_right (
        (* we backtrack each input, so the inputstate does not advance *)
        fun node ce -> CMatchExpr { matchee = (compile_node (inputstate,cap) node);
                                    patlist = [
                                      CCtor ("Error", [CName "e"]), ce;
                                      (* if not error, then return *)
                                      CName "success", CName "success"
                                    ]
                                  }
      ) es (CCtor ("Error", [CName "e"]))
  | Sequence es ->
    let (atext, nodes) = (
      match (List.rev es) with
      | (Action atext) :: nodes_rev -> (atext, List.rev nodes_rev)
      | _ -> ("(* missing action *)", es)
    ) in
    let (cexpr,_) = List.fold_right (
        fun node (ce, is) -> (
            CMatchExpr { matchee = (compile_node (is-1,cap) node);
                         patlist = [
                           CCtor ("Success",
                                  [value_n is; input_n is]), ce;
                           CName "e", CName "e"
                         ]
                       },
            (is - 1)
          )
      ) nodes (CVerb atext, inputstate + List.length nodes)
    in cexpr
  | PeekFor expr -> CVerb "(* PeekFor *)"
  | PeekNot expr -> CVerb "(* PeekNot *)"
  | Optional expr
    -> CMatchExpr { matchee = (compile_node (inputstate,cap) expr);
                    patlist = [
                      CCtor ("Success",
                             [value_n (inputstate+1);
                              input_n (inputstate+1)]),
                      CCtor ("Success",
                             [ CCtor ("Some", [value_n (inputstate+1)]);
                              input_n (inputstate+1)])
                      ;
                      CCtor ("Error", [CName "_"]),
                      CCtor ("Success",
                             [ CCtor ("None", []);
                              input_n (inputstate)])
                      ;
                    ]
                  }
  | Repeat expr -> CVerb "(* Repeat *)"
  | NonEmptyRepeat expr -> CVerb "(* NonEmptyRepeat *)"
  | Capture expr -> (compile_node (inputstate,true) expr)
  | Name (name, None) -> CApp (CName name, input_n inputstate)
  | Name (name, Some varname)
    -> (*FIXME: Do something with the varname *)
    CApp (CName name, input_n inputstate)
  | Literal lit -> CApp (CApp (CName "litmatch", CLit lit), input_n inputstate)
  | Class classlit -> CVerb "(* Class *)"
  | Any -> CApp (CName "read_any", input_n inputstate)
  | Action text ->
    (* Maybe want to do some "let varname=value1 in..." magic *)
    CVerb ("(" ^ text ^ ")")
  | Predicate text ->
    CVerb ("if (" ^ text ^ ") then Success ((), input" ^ (string_of_int inputstate) ^ ") Error \"custom predicate failed\"")


let rec compile_rule = function
  | Tree.Rule (name, expr) -> (
      let compiled_node = compile_node (0,false) expr in
      let as_string = string_of_caml compiled_node in
      Printf.printf "and %s input0 = (\n%s\n)\n" name as_string
    )
  | _ -> assert false

let compile_rules (rules : Tree.expr list) =
  (* TODO: create a rule lookup *)
  (* TODO: check for unmatched names *)
  (* TODO: check for left recursion *)
  let () = Printf.printf "type 'a result = Success of 'a * string | Error of string\n;;\n\n" in
  (* let _ = print_actions rules in *)
  let () = Printf.printf "let rec _stub=()\n" in
  List.iter compile_rule rules

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
