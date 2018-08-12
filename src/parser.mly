%{

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

%}


%token LPERCENT
%token RPERCENT
%token EQUAL
%token COLON
%token SEMICOLON
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
%token LBRACE
%token RBRACE

%token <string> IDENT
%token <string> CLASS
%token <string> LITERAL

%token <string> DECLARATION
%token <string> TRAILER

%token EOF

%type <grammar> grammar
%type <section> declaration trailer definition
%type <expr> expression

%start grammar

%%

grammar:
  ds=nonempty_list(declaration | definition {$1}) t = trailer? EOF {
    match t with
    | Some trail -> ds @ [trail]
    | None -> ds
  }
;
declaration:
  d=DECLARATION { Declaration d }
;
trailer:
  t=TRAILER { Trailer t }
;
definition:
  n=IDENT COLON e=expression SEMICOLON { Definition (Rule (n,e)) }
;
expression:
  a = separated_nonempty_list(BAR, sequence) { Alternate a }
;
sequence:
  s = prefix* { Sequence s }
;
prefix:
  s=suffix { s }
| AMP s=suffix { PeekNot s }
| NOT s=suffix  { PeekNot s }
;
suffix:
  p=primary { p }
| p=primary QUESTION { Optional p }
| p=primary STAR { Repeat p }
| p=primary PLUS { NonEmptyRepeat p }
;
primary:
  i=IDENT (* !: *) { Name i }
| LPAREN e=expression RPAREN { e }
| l=LITERAL { Literal l }
| c=CLASS { Class c }
| DOT { Any }

%%
