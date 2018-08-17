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
    | Parser.Error -> Printf.eprintf "It's all gone wrong\n"; exit 1
    | Lexer.SyntaxError msg -> Printf.eprintf "%s\n" msg; exit 1
  with e ->
    close_in_noerr channel;
    raise e

let print_result result =
  List.iter
    (function
      | Tree.Declaration s -> Printf.printf "%%{%s%%}" s
      | Tree.Definition e -> Printf.printf "%s\n" @@ Tree.string_of_expr e
      | Tree.Trailer s -> (print_endline "%%" ; print_endline s))
    result

let default default_value =
  function
  | Some value -> value
  | None -> default_value


type caml_expr =
  | CMatchExpr of {
      matchee : caml_expr;
      patlist : (caml_expr * caml_expr) list
    }
  | CName of string
  | CLit of string
  | CCtor of string * caml_expr list
  | CVerb of string (* verbatim ocaml output  *)
  | CTuple of caml_expr list
  | CApp of caml_expr * caml_expr
  | CLetRec of {
      name : string;
      args : caml_expr list;
      def : caml_expr;
      subexpr : caml_expr
    }
  | CList of caml_expr list
;;

let spaces n =
  let b = Bytes.create n in
  BytesLabels.fill b ~pos:0 ~len:n ' ';
  Bytes.to_string b

let rec string_of_caml ilvl =
  let nl = "\n" ^ spaces ilvl in
  let nl1 = nl ^ "  " in
  function
  | CMatchExpr { matchee; patlist } ->
    let strpat (p,e) =
      (nl ^ "| " ^ (string_of_caml ilvl p) ^ " -> (" ^ nl1 ^ (string_of_caml (ilvl+2) e) ^ nl ^ ")")
    in
    sprintf "match (%s%s%s) with%s" nl1
      (string_of_caml (ilvl+2) matchee) nl @@ String.concat "" @@ List.map strpat patlist
  | CName name -> name
  | CLit lit -> sprintf "\"%s\"" @@ String.escaped lit
  | CCtor (ctor, cexprs) -> (
      match cexprs with
      | [] -> sprintf "%s" ctor
      | _ -> sprintf "%s (%s)" ctor @@ string_of_caml ilvl (CTuple cexprs)
    )
  | CVerb verbatim -> verbatim
  | CTuple cexprs -> String.concat ", " @@ List.map (string_of_caml ilvl) cexprs
  | CApp (e1, e2) -> sprintf "(%s) (%s)" (string_of_caml ilvl e1) (string_of_caml ilvl e2)
  | CLetRec { name; args; def; subexpr } ->
    sprintf "let rec %s %s = (%s%s%s) in (%s%s%s)"
      name (String.concat " " @@  List.map (string_of_caml ilvl) args)
      nl1 (string_of_caml (ilvl+2) def) nl nl1 (string_of_caml (ilvl+2) subexpr) nl
  | CList exprs -> "[" ^ (String.concat "; " @@ List.map (string_of_caml ilvl) exprs) ^"]"


module SetString = Set.Make(String)
module MapString = Map.Make(String)

let nasty_global_classfn_lookup = ref []

(**
 * Given a sequence with variables and an action:
 *     v1=expr1 v2=expr2 v3=expr3 { v1 + v2 + v3 }
 * replace matching identifiers in the action with the matched values from
 * the sequence
 *)
let sub_vars_text varmap inputstate text =
  (* In the default Genlex lexer . is illegal *)
  let lexer = Genlex.make_lexer ["."] in
  let toks = lexer (Stream.of_string text) in
  let reversed_results = ref [] in
  let prepend s = reversed_results := s :: !reversed_results in
  let () =
    Stream.iter
      (fun token ->
        match token with
        | Genlex.Ident id ->
          (match List.assoc_opt id varmap with
            | Some offset ->
                prepend @@ "yyValue" ^ string_of_int (inputstate + offset)
            | None -> prepend id)
        | Genlex.Char c -> prepend ("'" ^ Char.escaped c ^ "'")
        | Genlex.String s -> prepend ("\"" ^ String.escaped s ^ "\"")
        | Genlex.Kwd kwd -> prepend kwd
        | Genlex.Int i -> prepend @@ string_of_int i
        | Genlex.Float f -> prepend @@ string_of_float f)
      toks
  in
  String.concat " " @@ List.rev !reversed_results
;;

(**
 * inputstate tracks the variable name of the input we are going to try to
 * match against.
 *)
let rec compile_node (inputstate, capture) =
  let open Tree in
  let statevar name n = CName (sprintf "%s%d" name n) in
  let input_n = statevar "yyInput" in
  let value_n = statevar "yyValue" in
  function
  | Rule (_,_) -> assert false
  | Alternate es (* chain matches on the error case *)
    -> List.fold_right
        (* we backtrack each input, so the inputstate does not advance *)
         (fun node caml_expr ->
            CMatchExpr {
              matchee = (compile_node (inputstate,capture) node);
              patlist = [
                CCtor ("Error", [CName "e"]), caml_expr;
                (* if not error, then return *)
                CName "success", CName "success" ]
            })
         es (CCtor ("Error", [CName "e"]))
  | Sequence es ->
    let (varmap, _) =
      List.fold_left
        (fun (varmap, i) e ->
           match e with
           | Name (_, Some varname) -> (varname, i) :: varmap, i+1
           | _ -> varmap, i+1
        )
        ([], 1) es
    in
    let sub_vars_node = function
      | Action text -> Action (sub_vars_text varmap inputstate text)
      | other -> other
    in
    let final_ist = (inputstate + List.length es - 1) in
    let (first, nodes) = (
      match (List.rev es) with
      | first :: nodes_rev ->
        let compiled_first =
          compile_node (final_ist, capture) (sub_vars_node first)
        in
        (compiled_first, List.rev nodes_rev)
      | [] -> (CCtor ("Success", [value_n inputstate; input_n inputstate]), [])
    ) in
    let (cexpr, _) =
      List.fold_right
        (fun node (caml_expr, ist) ->
           (CMatchExpr {
               matchee = (compile_node (ist-1,capture) (sub_vars_node node));
               patlist = [
                 (CCtor ("Success", [value_n ist; input_n ist]), caml_expr);
                 (CCtor ("Error", [CName "e"]),
                  CCtor ("Error", [CName "e"]))
               ]
             },
            (ist - 1)))
        nodes (first, inputstate + List.length nodes)
    in cexpr
  | PeekFor expr -> CVerb "(* PeekFor *)"
  | PeekNot expr -> CVerb "(* PeekNot *)"
  | Optional expr
    -> CMatchExpr {
        matchee = (compile_node (inputstate,capture) expr);
        patlist = [
          CCtor ("Success", [value_n (inputstate+1);
                             input_n (inputstate+1)]),
          CCtor ("Success", [CCtor ("Some", [value_n (inputstate+1)]);
                             input_n (inputstate+1)])
          ;
          CCtor ("Error", [CName "_"]),
          CCtor ("Success", [CCtor ("None", []); input_n (inputstate)])
          ;
        ]
      }
  | Repeat expr ->
    CLetRec {
      name = "aux";
      args = [CName "res"; CName "yyInput0"];
      def = CMatchExpr {
          matchee = (compile_node (0,capture) expr);
          patlist = [
            CVerb "Success (v, i1)", CVerb "aux (v :: res) i1" ;
            CVerb "Error _", CVerb "Success (res, yyInput0)" ;
          ]
        };
      subexpr = CApp (CApp (CName "aux", CList []), input_n inputstate)
    }
  | NonEmptyRepeat expr
    -> compile_node (inputstate, capture) @@ Sequence [expr; Repeat expr]
  | Capture expr -> (compile_node (inputstate, true) expr)
  | Name (name, None) -> CApp (CName name, input_n inputstate)
  | Name (name, Some varname)
    -> (*FIXME: Do something with the varname *)
    CApp (CName name, input_n inputstate)
  | Literal lit -> CApp (CApp (CName "litmatch", CLit lit), input_n inputstate)
  | Class classlit ->
    let matchfn_name =
      List.assoc classlit !nasty_global_classfn_lookup
    in
    CApp (CApp (CName "classmatch", CName matchfn_name), input_n inputstate)
  | Any -> CApp (CName "read_any", input_n inputstate)
  | Action text ->
    (* Maybe want to do some "let varname=value1 in..." magic *)
    CCtor ("Success", [CVerb ("(" ^ text ^ ")"); input_n inputstate])
  | Predicate text ->
    CVerb ("if (" ^ text ^ ") then Success ((), yyInput" ^ (string_of_int inputstate) ^ ") else Error \"custom predicate failed\"")


let compile_class_pos classlit =
  let lexbuf = Lexing.from_string classlit in
  let next_token () = Lexer.class_token lexbuf in
  let rec aux results =
    let return s =
      aux (s :: results) @@ next_token ()
    in
    function
    | Lexer.RangeTok (cstart, cend) -> (
        if cstart > cend then
          raise @@ Invalid_argument classlit;
        return @@ Printf.sprintf "('%s' <= c && c <= '%s')"
          (Char.escaped cstart) (Char.escaped cend)
      )
    | Lexer.CharTok c -> (
        return @@ Printf.sprintf "(c = '%s')" @@ Char.escaped c
      )
    | Lexer.EofTok -> results
  in
  let expr_strings = aux [] @@ next_token () in
  String.concat " || " expr_strings


let compile_class classlit =
  let len = String.length classlit in
  if len > 1 && classlit.[0] = '^' then
    "not (" ^ compile_class_pos (Stringext.string_after classlit 1 ) ^ ")"
  else
    compile_class_pos classlit


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
      let as_string = string_of_caml 2 compiled_node in
      Printf.printf "and %s yyInput0 = (\n  %s\n)\n" name as_string
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
  if lit_len > len then
    Error \"End of input\"
  else
    let rec aux pos =
      if pos = lit_len then
        true
      else if literal.[pos] = str.[pos + off] then
        aux (pos + 1)
      else
        false
    in
    if (aux 0) then
      Success (literal, (str, off+lit_len, len-lit_len))
    else
      Error literal

let classmatch matchfn (str, off, len) =
  if len = 0 then
    Error \"End of input \"
  else if matchfn (str.[off]) then
    Success (str.[off], (str, off+1, len-1))
  else
    Error \"nomatch\"

(*TODO: implement read_any *)

" in
  let classes = List.fold_left compile_classes [] rules in
  let (_, unique_classes) =
    List.fold_left
      (fun (set, mem) (lit, body) ->
         if SetString.mem lit set then
           (set, mem)
         else
           (SetString.add lit set, (lit, body) :: mem))
      (SetString.empty, []) classes
  in
  let (classlit2funcname, _) =
    List.fold_left
      (fun (map, idx) (classlit, classbody) ->
         Printf.printf "let yyClassmatch%d c = %s\n\n" idx classbody;
         (classlit, Printf.sprintf "yyClassmatch%d" idx) :: map, idx + 1)
      ([], 0) unique_classes
  in
  let () = nasty_global_classfn_lookup := classlit2funcname in
  let () = Printf.printf "let rec _stub=()\n" in
  List.iter compile_rule rules

let compile_result result =
  let open Tree in
  (* Print headers *)
  let () =
    List.iter
      (function
        | Declaration declaration -> Printf.printf "%s" declaration
        | Definition _ -> ()
        | Trailer _ -> ())
      result
  in
  (* compile nodes *)
  let rules =
    List.fold_right
      (* filter_map *)
      (fun section xs -> match section with
         | Definition rule -> rule :: xs
         | _ -> xs)
      result []
  in compile_rules rules;
  (* dump trailer *)
  List.iter
    (function
      | Declaration _ -> ()
      | Definition _ -> ()
      | Trailer trailer -> Printf.printf "%s" trailer)
    result


let () =
  match (Array.to_list Sys.argv) with
  | [] -> Printf.eprintf("unreachable")
  | progname :: [] ->
      (Printf.eprintf "usage: %s <filename>\n" progname; exit 1)
  | progname :: filename :: _ -> begin
      parse filename compile_result
    end
