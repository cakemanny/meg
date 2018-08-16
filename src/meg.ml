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
  | CLetRec of { name : string;
                 args : caml_expr list;
                 def : caml_expr;
                 subexpr : caml_expr }
  | CList of caml_expr list
;;


let rec string_of_caml = function
  | CMatchExpr { matchee; patlist } ->
    let strpat (p,e) =
      ("| " ^ (string_of_caml p) ^ " -> (" ^ (string_of_caml e) ^ ")")
    in
    sprintf "match (%s) with %s"
      (string_of_caml matchee) @@ String.concat "" @@ List.map strpat patlist
  | CName name -> name
  | CLit lit -> sprintf "\"%s\"" @@ String.escaped lit
  | CCtor (ctor, cexprs) ->
    sprintf "%s (%s)" ctor @@ string_of_caml (CTuple cexprs)
  | CVerb verbatim -> verbatim
  | CTuple cexprs -> String.concat "," @@ List.map string_of_caml cexprs
  | CApp (e1, e2) -> sprintf "(%s) (%s)" (string_of_caml e1) (string_of_caml e2)
  | CLetRec { name; args; def; subexpr } ->
    sprintf "let rec %s %s = (%s) in (%s)"
      name (String.concat " " @@  List.map string_of_caml args)
      (string_of_caml def) (string_of_caml subexpr)
  | CList exprs -> "[" ^ (String.concat "; " @@ List.map string_of_caml exprs) ^"]"


let nasty_global_classfn_lookup = ref []

(**
 * inputstate tracks the variable name of the input we are going to try to
 * match against.
 *)
let rec compile_node (inputstate, capture) =
  let open Tree in
  let input_n n = CName ("input" ^ (string_of_int n)) in
  let value_n n = CName ("value" ^ (string_of_int n )) in
  function
  | Rule (_,_) -> assert false
  | Alternate es (* chain matches on the error case *)
    -> List.fold_right (
        (* we backtrack each input, so the inputstate does not advance *)
        fun node ce -> CMatchExpr { matchee = (compile_node (inputstate,capture) node);
                                    patlist = [
                                      CCtor ("Error", [CName "e"]), ce;
                                      (* if not error, then return *)
                                      CName "success", CName "success"
                                    ]
                                  }
      ) es (CCtor ("Error", [CName "e"]))
  | Sequence es ->
    let final_ist = (inputstate + List.length es - 1) in
    let (first, nodes) = (
      match (List.rev es) with
      | first :: nodes_rev ->
        let compiled_first = (compile_node (final_ist,capture) first)
        in (compiled_first, List.rev nodes_rev)
      | [] -> (CCtor ("Success", [value_n inputstate; input_n inputstate]), [])
    ) in
    let (cexpr, _) = List.fold_right (
        fun node (caml_expr, ist) -> (
            CMatchExpr { matchee = (compile_node (ist-1,capture) node);
                         patlist = [
                           CCtor ("Success",
                                  [value_n ist; input_n ist]), caml_expr;
                           CName "e", CName "e"
                         ]
                       },
            (ist - 1)
          )
      ) nodes (first, inputstate + List.length nodes)
    in cexpr
  | PeekFor expr -> CVerb "(* PeekFor *)"
  | PeekNot expr -> CVerb "(* PeekNot *)"
  | Optional expr
    -> CMatchExpr { matchee = (compile_node (inputstate,capture) expr);
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
  | Repeat expr ->
    CLetRec {
      name = "aux";
      args = [CName "res"; CName "input0"];
      def = CMatchExpr {
          matchee = (compile_node (0,capture) expr);
          patlist = [
            CVerb "Success (v, i1)", CVerb "aux (v :: res) i1" ;
            CVerb "Error _", CVerb "Success (res, input0)" ;
          ]
        };
      subexpr = CApp (CApp (CName "aux", CList []), input_n inputstate)
    }
  | NonEmptyRepeat expr
    -> compile_node (inputstate, capture) @@ Sequence [expr; Repeat expr]
  | Capture expr -> (compile_node (inputstate,true) expr)
  | Name (name, None) -> CApp (CName name, input_n inputstate)
  | Name (name, Some varname)
    -> (*FIXME: Do something with the varname *)
    CApp (CName name, input_n inputstate)
  | Literal lit -> CApp (CApp (CName "litmatch", CLit lit), input_n inputstate)
  | Class classlit ->
    let matchfn_name = List.assoc classlit !nasty_global_classfn_lookup in
    CApp (CApp (CName "classmatch", CName matchfn_name), input_n inputstate)
  | Any -> CApp (CName "read_any", input_n inputstate)
  | Action text ->
    (* Maybe want to do some "let varname=value1 in..." magic *)
    CVerb ("(" ^ text ^ ")")
  | Predicate text ->
    CVerb ("if (" ^ text ^ ") then Success ((), input" ^ (string_of_int inputstate) ^ ") else Error \"custom predicate failed\"")


let compile_class classlit =
  let lexbuf = Lexing.from_string classlit in
  let next_token () = Lexer.class_token lexbuf in
  let rec aux results =
    let return = (fun s -> aux (s :: results) @@ next_token ()) in
    function
    | Lexer.RangeTok (cstart, cend) -> (
        if cstart > cend then raise @@ Invalid_argument classlit;
        return @@ Printf.sprintf "('%c' <= c && c <= '%c')" cstart cend
      )
    | Lexer.CharTok (c) -> (
        return @@ Printf.sprintf "(c = '%c')" c
      )
    | Lexer.EofTok -> results
  in
  let expr_strings = aux [] @@ next_token () in
  String.concat " || " expr_strings


let rec compile_classes result_list =
  let open Tree in
  function
  | Rule (name, expr) -> compile_classes result_list expr
  | Alternate exprs
  | Sequence exprs
    -> List.fold_left compile_classes result_list exprs
  | PeekFor expr
  | PeekNot expr
  | Optional expr
  | Repeat expr
  | NonEmptyRepeat expr
  | Capture expr
    -> compile_classes result_list expr
  | Name (_, _)
  | Literal _
  | Any
  | Action _
  | Predicate _
    -> result_list
  | Class classlit -> (classlit, compile_class classlit) :: result_list


let compile_rule = function
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
  let () = Printf.printf "%s" "
type string_view = string * int * int
let string_view_of_string s = (s, 0, String.length s)

type 'a result = Success of 'a * string_view
               | Error of string

let litmatch literal (str, off, len) =
  let lit_len = String.length literal in
  if lit_len > len
  then Error \"End of input\"
  else let rec aux pos =
      if pos = lit_len then true
      else if literal.[pos] = str.[pos + off]
      then aux (pos + 1)
      else false
  in if (aux 0)
  then Success (literal, (str, off+lit_len, len-lit_len))
  else Error literal

let classmatch matchfn (str, off, len) =
  if len = 0
  then Error \"End of input \"
  else if matchfn (str.[off])
  then Success (str.[off], (str, off+1, len-1))
  else Error \"nomatch\"

(*TODO: implement read_any *)

" in
  (* let _ = print_actions rules in *)
  let classes = List.fold_left compile_classes [] rules in
  let classlit2funcname = List.fold_left (fun (map,idx) (classlit, classbody) ->
      Printf.printf "let yy_classmatch%d c = %s\n\n" idx classbody;
      (classlit, Printf.sprintf "yy_classmatch%d" idx) :: map, idx + 1
    ) ([], 0) classes
  in
  let () = nasty_global_classfn_lookup := fst classlit2funcname in
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
