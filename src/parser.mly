/* vim: set indentexpr=: */
%{

open Tree

let unclosed start_pos start_name end_pos end_name =
  raise Syntaxerr.(Error (Unclosed (start_pos, start_name, end_pos, end_name)))

(*
let expecting pos name =
  raise Syntaxerr.(Error (Expecting (pos, name)))
*)

let not_expecting pos name =
  raise Syntaxerr.(Error (NotExpecting (pos, name)))

let general_err pos msg =
  raise Syntaxerr.(Error (General (pos, msg)))

%}

%token EQUAL
%token COLON
%token SEMI
%token BAR
%token AMP
%token NOT
%token QUESTION
%token STAR
%token PLUS
%token LPAREN
%token RPAREN
%token DOT
%token LT
%token GT

%token <string> IDENT
%token <string> CLASS
%token <string> LITERAL
%token <string> BRACES

%token <string * Lexing.position * Lexing.position> DECLARATION TRAILER

%token EOF

%type <Tree.grammar> grammar
%type <Tree.section> declaration trailer definition
%type <Tree.expr> expression

%start grammar

%%

grammar:
    ds=nonempty_list(declaration | definition {$1}) t=trailer? EOF {
      match t with
      | Some trail -> ds @ [trail]
      | None -> ds
    }
;
declaration:
    d=DECLARATION { let (cnts,startp,endp) = d in Declaration (cnts,startp,endp) }
;
trailer:
    t=TRAILER { let (cnts,startp,endp) = t in Trailer (cnts,startp,endp) }
;
definition:
    n=def_start e=expression SEMI? { Definition (Rule (n,e)) }
  | COLON { not_expecting $startpos($1) ":" }
  | error { general_err $startpos "a definition was bad!" }
;
def_start:
  n=IDENT COLON { n }
;
expression:
    a=separated_nonempty_list(BAR, sequence) { match a with
                                               | [only_alt] -> only_alt
                                               | a -> Alternate a }
;
sequence:
    s=prefix* { Sequence s }
;
prefix:
    AMP pred=BRACES { Predicate pred }
  | AMP s=suffix_no_action { PeekFor s }
  | NOT s=suffix_no_action { PeekNot s }
  | n=IDENT EQUAL s=suffix { Assign (n, s) }
  | s=suffix { s }
;
suffix:
    p=primary { p }
  | p=primary QUESTION { Optional p }
  | p=primary STAR { Repeat p }
  | p=primary PLUS { NonEmptyRepeat p }
;
primary:
    p=primary_no_action { p }
  | text=BRACES { Action text  }
;
/* these are to resolve a reduce/reduce conflict between predicates and
   peeking an action. it doesn't really make sense to peek or peeknot an action
   but you can still do it by parenthesising the expression
 */
suffix_no_action:
    p=primary_no_action { p }
  | p=primary_no_action QUESTION { Optional p }
  | p=primary_no_action STAR { Repeat p }
  | p=primary_no_action PLUS { NonEmptyRepeat p }
;
primary_no_action:
    name=IDENT (* !: *) { Name name }
  | LPAREN e=expression RPAREN { e }
  | LPAREN expression error { unclosed $startpos($1) "(" $startpos($3) ")" }
  | l=LITERAL { Literal l }
  | c=CLASS { Class c }
  | DOT { Any }
  | LT e=expression GT { Capture e }
  | LT expression error { unclosed $startpos($1) "<" $startpos($3) ">" }
;
%%
