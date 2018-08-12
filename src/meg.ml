(* module T = Tree *)

let parse_and_print filename =
  let channel = open_in filename in
  try
    let lexbuf = Lexing.from_channel channel in
    let lexbuf = Lexer.set_filename filename lexbuf in
    try
      let result = Parser.grammar Lexer.token lexbuf in
      List.iter (
        function
        | Tree.Declaration s -> Printf.printf "%%{%s%%}" s
        | Tree.Definition e -> Printf.printf "%s\n" @@ Tree.string_of_expr e
        | Tree.Trailer s -> (print_endline "%%" ; print_endline s)
      ) result
    with
    | Parser.Error -> Printf.eprintf "It's all gone wrong\n"
    | Lexer.SyntaxError msg -> Printf.eprintf "%s\n" msg
  with e ->
    close_in_noerr channel;
    raise e


let () =
  match (Array.to_list Sys.argv) with
  | [] -> Printf.eprintf("unreachable")
  | progname :: [] -> Printf.eprintf "usage: %s <filename>\n" progname
  | progname :: filename :: _ -> parse_and_print filename
