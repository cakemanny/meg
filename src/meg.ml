
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
      | Tree.Declaration (s,_,_) -> Printf.printf "%%{%s%%}" s
      | Tree.Definition e -> Printf.printf "%s\n" @@ Tree.string_of_expr e
      | Tree.Trailer (s,_,_) -> (print_endline "%%" ; print_endline s))
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
  | CLet of {
      pat : caml_expr;
      def : caml_expr;
      subexpr : caml_expr
    }
  | CLetRec of {
      name : string;
      args : caml_expr list;
      def : caml_expr;
      subexpr : caml_expr
    }
  | CList of caml_expr list (* A list literal *)
  | CSeq of caml_expr list (* A sequence of statements ; separated *)


let spaces n =
  let b = Bytes.create n in
  BytesLabels.fill b ~pos:0 ~len:n ' ';
  Bytes.to_string b

let rec string_of_caml ilvl =
  let nl = "\n" ^ spaces ilvl in
  let nl1 = nl ^ "  " in
  function
  | CMatchExpr { matchee; patlist } ->
    let strpat (p, e) =
      nl ^ "| " ^ (string_of_caml ilvl p) ^ " -> (" ^
      nl1 ^ (string_of_caml (ilvl+2) e) ^
      nl ^ ")"
    in
    sprintf "match (%s%s%s) with%s"
      nl1 (string_of_caml (ilvl+2) matchee) nl
      @@ String.concat "" @@ List.map strpat patlist
  | CName name -> name
  | CLit lit -> sprintf "\"%s\"" @@ String.escaped lit
  | CCtor (ctor, cexprs) -> (
      match cexprs with
      | [] -> sprintf "%s" ctor
      | _ -> sprintf "%s %s" ctor @@ string_of_caml ilvl (CTuple cexprs)
    )
  | CVerb verbatim -> verbatim
  | CTuple cexprs ->
    "(" ^ (String.concat ", " @@ List.map (string_of_caml ilvl) cexprs) ^ ")"
  | CApp (e1, e2) ->
    sprintf "%s (%s)"
      (string_of_caml ilvl e1)
      (string_of_caml ilvl e2)
  | CLet { pat; def; subexpr } ->
    sprintf "let %s = (%s%s%s) in (%s%s%s)"
      (string_of_caml ilvl pat)
      nl1 (string_of_caml (ilvl+2) def) nl
      nl1 (string_of_caml (ilvl+2) subexpr) nl
  | CLetRec { name; args; def; subexpr } ->
    sprintf "let rec %s %s = (%s%s%s) in (%s%s%s)"
      name
      (String.concat " " @@  List.map (string_of_caml ilvl) args)
      nl1 (string_of_caml (ilvl+2) def) nl
      nl1 (string_of_caml (ilvl+2) subexpr) nl
  | CList exprs ->
    "[" ^ (String.concat "; " @@ List.map (string_of_caml ilvl) exprs) ^ "]"
  | CSeq exprs ->
    "(" ^ nl1 ^
    (String.concat "; " @@ List.map (string_of_caml (ilvl+2)) exprs) ^
    nl ^ ")"


module SetString = Set.Make(String)
module MapString = Map.Make(String)

let nasty_global_classfn_lookup = ref []


let define_vars_text varmap inputstate text =
  varmap
  |> List.filter
    (fun (varname,_) ->
       let poss_ids = Str.(split (regexp "[^A-Za-z0-9_]+") text) in
       List.mem varname poss_ids)
    (* The varmap appears to be in rev order (created by fold_left)  *)
  |> List.rev_map (fun (varname, offset) ->
        sprintf "let %s = yyValue%d in" varname (inputstate + offset))
  |> (fun xs -> xs @ [text])
  |> String.concat " "


(* We allow hyphens in identifiers, which are not valid ocaml names
 * We allow uppercase rulenames - they don't get exported *)
let to_rule_name =
  let handle_dashes =
    function
    | "-" -> "yy_blank"
    | name -> Stringext.replace_all name ~pattern:"-" ~with_:"_"
  in
  let handle_uppercase name =
    if name.[0] = Char.uppercase_ascii name.[0] then
      "yy_" ^ name
    else
      name
  in
  fun name -> handle_uppercase @@ handle_dashes name


(**
 * inputstate tracks the variable name of the input we are going to try to
 * match against.
 *)
let rec compile_node inputstate =
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
              matchee = compile_node inputstate node;
              patlist = [
                CCtor ("Error", [CName "e"]), caml_expr;
                (* if not error, then return *)
                CName "success", CName "success"
              ]
            })
         es (CCtor ("Error", [CName "e"]))
  | Sequence es ->
    let (varmap, _) =
      List.fold_left
        (fun (varmap, i) e ->
           match e with
           | Assign (varname, _) -> (varname, i) :: varmap, i+1
           | _ -> varmap, i+1
        )
        ([], 1) es
    in
    let define_vars_node = function
      | Action text -> Action (define_vars_text varmap inputstate text)
      | Predicate text -> Predicate (define_vars_text varmap inputstate text)
      | other -> other
    in
    let final_ist = (inputstate + List.length es - 1) in
    let (first, nodes) = (
      match (List.rev es) with
      | first :: nodes_rev ->
        let compiled_first =
          compile_node final_ist (define_vars_node first)
        in
        (compiled_first, List.rev nodes_rev)
      | [] ->
        (CCtor ("Ok", [value_n inputstate; input_n inputstate]),
         [])
    ) in
    let (cexpr, _) =
      List.fold_right
        (fun node (caml_expr, ist) ->
           (CMatchExpr {
               matchee = compile_node (ist-1) (define_vars_node node);
               patlist = [
                 (CCtor ("Ok", [value_n ist; input_n ist]), caml_expr);
                 (CCtor ("Error", [CName "e"]),
                  CCtor ("Error", [CName "e"]))
               ]
             },
            (ist - 1)))
        nodes (first, inputstate + List.length nodes)
    in cexpr
  | PeekFor expr ->
    CMatchExpr {
        matchee = compile_node inputstate expr;
        patlist = [
          CCtor ("Ok", [CName "_"; CName "_"]),
          CCtor ("Ok", [CName "()"; input_n inputstate])
          ;
          CCtor ("Error", [CName "e"]),
          CCtor ("Error", [CName "e"])
          ;
        ]

    }
  | PeekNot expr ->
    CMatchExpr {
        matchee = compile_node inputstate expr;
        patlist = [
          CCtor ("Error", [CName "_"]),
          CCtor ("Ok", [CName "()"; input_n inputstate])
          ;
          CCtor ("Ok", [CName "_"; CName "_"]),
          CCtor ("Error", [CVerb "\"Expected not to match ...\""])
          ;
        ]
    }
  | Optional expr
    -> CMatchExpr {
        matchee = compile_node inputstate expr;
        patlist = [
          CCtor ("Ok", [value_n (inputstate+1); input_n (inputstate+1)]),
          CCtor ("Ok", [CCtor ("Some", [value_n (inputstate+1)]);
                        input_n (inputstate+1)])
          ;
          CCtor ("Error", [CName "_"]),
          CCtor ("Ok", [CCtor ("None", []); input_n inputstate])
          ;
        ]
      }
  | Repeat expr ->
    CLetRec {
      name = "aux";
      args = [CName "res"; CName "yyInput0"];
      def = CMatchExpr {
          matchee = compile_node 0 expr;
          patlist = [
            CVerb "Ok (v, i1)", CVerb "aux (v :: res) i1" ;
            CVerb "Error _", CVerb "Ok (res, yyInput0)" ;
          ]
        };
      subexpr = CApp (CApp (CName "aux", CList []), input_n inputstate)
    }
  | NonEmptyRepeat expr
    -> compile_node inputstate @@ Sequence [expr; Repeat expr]
  | Capture expr ->
    CMatchExpr {
      matchee = compile_node inputstate expr;
      patlist = [
        (CCtor ("Ok", [ CName "v"; CName "remaining_input" ]),
         CLet {
           pat = CTuple [ CName "_"; CName "off_start"; CName "_"; CName "_" ];
           def = input_n inputstate;
           subexpr =
             CLet {
               pat = CTuple [ CName "str"; CName "off_end"; CName "len"; CName "_" ];
               def = CName "remaining_input";
               subexpr =
                 CCtor (
                   "Ok", [
                     CName "v";
                     CTuple [
                       CName "str"; CName "off_end"; CName "len";
                       CVerb "String.sub str off_start (off_end - off_start)"
                     ]
                   ]
                 )
             }
         });
        (CCtor ("Error", [CName "e"]),
         CCtor ("Error", [CName "e"]) )
      ]
    }
  | Assign (_, expr) -> compile_node inputstate expr
  | Name name ->
    CApp (CName (to_rule_name name), input_n inputstate)
  | Literal lit ->
    CApp (CApp (CName "yyMatchLiteral", CLit lit), input_n inputstate)
  | Class classlit ->
    let matchfn_name =
      List.assoc classlit !nasty_global_classfn_lookup
    in
    CApp (CApp (CName "yyMatchClass", CName matchfn_name), input_n inputstate)
  | Any -> CApp (CName "yyMatchAny", input_n inputstate)
  | Action text ->
    let action_expr =
      (* TODO: spit out source file line numbers *)
      CCtor ("Ok", [CVerb ("(" ^ text ^ ")"); input_n inputstate])
    in
    (match Stringext.find_from text ~pattern:"yytext" with
     | Some _ ->
       CLet {
         pat = CTuple [CName "_"; CName "_"; CName "_"; CName "yytext" ];
         def = input_n inputstate;
         subexpr = action_expr;
       }
     | None -> action_expr)
  | Predicate text ->
    let action_expr =
      (* TODO: spit out source file line numbers *)
      CVerb (
        "if (" ^ text ^ ") " ^
        "then Ok ((), yyInput" ^ (string_of_int inputstate) ^ ") " ^
        "else Error \"custom predicate failed: " ^ String.escaped text ^ "\""
      )
    in
    (match Stringext.find_from text ~pattern:"yytext" with
     | Some _ ->
       CLet {
         pat = CTuple [CName "_"; CName "_"; CName "_"; CName "yytext" ];
         def = input_n inputstate;
         subexpr = action_expr;
       }
     | None -> action_expr)


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
        return @@ sprintf "('%s' <= c && c <= '%s')"
          (Char.escaped cstart) (Char.escaped cend)
      )
    | Lexer.CharTok c -> (
        return @@ sprintf "(c = '%s')" @@ Char.escaped c
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
  | Assign (_,expr)
    -> compile_classes result_list expr
  | Name _
  | Literal _
  | Any
  | Action _
  | Predicate _
    -> result_list
  | Class classlit -> (classlit, compile_class classlit) :: result_list


let compile_rule = function
  | Tree.Rule (name, expr) ->
      let compiled_node = compile_node 0 expr in
      let as_string = string_of_caml 2 compiled_node in
      Printf.printf "and %s yyInput0 = (\n  %s\n)\n" (to_rule_name name) as_string
  | _ -> assert false


let compile_rules (rules : Tree.expr list) =
  (* TODO: create a rule lookup *)
  let () = Printf.printf "%s" "
type string_view = string * int * int * string

let string_view_of_string s = (s, 0, String.length s, \"\")

type 'a parse_result = ('a * string_view, string) result

let yyMatchLiteral literal (str, off, len, yytext) : string parse_result =
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
      Ok (literal, (str, off+lit_len, len-lit_len, yytext))
    else
      Error literal

let yyMatchClass matchfn (str, off, len, yytext) : char parse_result =
  if len = 0 then
    Error \"End of input\"
  else if matchfn (str.[off]) then
    Ok (str.[off], (str, off+1, len-1, yytext))
  else
    Error \"nomatch\"

let yyMatchAny = yyMatchClass (fun _ -> true)

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
         Printf.printf "let yyClassMatcher%d c = %s\n\n" idx classbody;
         (classlit, sprintf "yyClassMatcher%d" idx) :: map, idx + 1)
      ([], 0) unique_classes
  in
  let () = nasty_global_classfn_lookup := classlit2funcname in
  let () = Printf.printf "let rec _stub = ()\n" in
  List.iter compile_rule rules


let rule_names rules =
  List.map
    (function
      | Tree.Rule (name, _) -> name
      | _ -> assert false)
    rules


let check_for_duplicate (rules: Tree.expr list) =
  let names : string list = rule_names rules in
  let (dups, _) =
    List.fold_left
      (fun (errs, xs) x ->
         if List.mem x xs then
           (x :: errs, xs)
         else
           (errs, x :: xs))
      ([], [])
      names
  in
  dups


let check_for_undefined (rules: Tree.expr list) =
  let open Tree in
  let names = rule_names rules in
  let rec check_rule =
    function
    | Alternate exprs
    | Sequence exprs -> List.iter check_rule exprs
    | Rule (_, expr)
    | PeekFor expr
    | PeekNot expr
    | Optional expr
    | Repeat expr
    | NonEmptyRepeat expr
    | Capture expr
    | Assign (_, expr)-> check_rule expr
    | Name name ->
      if List.mem name names = false
      then (Printf.eprintf "%s is not defined!\n" name; exit 1)
    | Literal _
    | Class _
    | Any
    | Action _
    | Predicate _ -> ()
  in List.iter check_rule rules


let compile_result result =
  let open Tree in
  let rules =
    List.fold_right
      (* filter_map *)
      (fun section xs ->
         match section with
         | Definition rule -> rule :: xs
         | _ -> xs)
      result []
  in
  let () = check_for_undefined rules in
  let duplicate_rule_names = check_for_duplicate rules in
  let () = if List.length duplicate_rule_names > 0 then
      (List.iter
         (fun name -> Printf.eprintf "Duplicate rules with name %s\n" name)
         duplicate_rule_names;
       exit 1)
  in
  (* TODO: check for left recursion *)
  (* Print headers *)
  let () =
    List.iter
      (function
        | Declaration (declaration,sp,ep) ->
          (Printf.printf "# %d \"%s\"\n" sp.pos_lnum sp.pos_fname;
           Printf.printf "%s\n" declaration;
           (* TODO: track output filename and line numbers! *)
           Printf.printf "# %d \"%s\"\n" 0 "<stdout>")
        | Definition _ -> ()
        | Trailer _ -> ())
      result
  in
  (* compile nodes *)
  compile_rules rules;
  (* dump trailer *)
  List.iter
    (function
      | Declaration (_,_,_) -> ()
      | Definition _ -> ()
      | Trailer (trailer, sp, ep) ->
          (Printf.printf "# %d \"%s\"\n" sp.pos_lnum sp.pos_fname;
           Printf.printf "%s\n" trailer;
           Printf.printf "# %d \"%s\"\n" 0 "<stdout>"))
    result


let () =
  match (Array.to_list Sys.argv) with
  | [] -> assert false
  | progname :: [] ->
      (Printf.eprintf "usage: %s <filename>\n" progname; exit 1)
  | progname :: filename :: _ -> begin
      parse filename compile_result
    end
