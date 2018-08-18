%{

open Tree

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
  ds=nonempty_list(declaration | definition {$1}) t = trailer? EOF {
    match t with
    | Some trail -> ds @ [trail]
    | None -> ds
  }
| error { Printf.eprintf("There was an error in your syntax\n"); exit 1; [] }
;
declaration:
  d=DECLARATION { let (cnts,startp,endp) = d in Declaration (cnts,startp,endp) }
;
trailer:
  t=TRAILER { let (cnts,startp,endp) = t in Trailer (cnts,startp,endp) }
;
definition:
  n=IDENT COLON e=expression SEMI? { Definition (Rule (n,e)) }
;
expression:
  a = separated_nonempty_list(BAR, sequence) { match a with
                                               | [only_alt] -> only_alt
                                               | a -> Alternate a }
;
sequence:
  s = prefix* { Sequence s }
;
prefix:
  AMP pred=BRACES { Predicate pred }
| AMP s=suffix { PeekNot s }
| NOT s=suffix  { PeekNot s }
| s=suffix { s }
;
suffix:
  p=primary { p }
| p=primary QUESTION { Optional p }
| p=primary STAR { Repeat p }
| p=primary PLUS { NonEmptyRepeat p }
;
primary:
  varname=IDENT EQUAL name=IDENT (* !: *) { Name (name, Some varname) }
| name=IDENT (* !: *) { Name (name, None) }
| LPAREN e=expression RPAREN { e }
| l=LITERAL { Literal l }
| c=CLASS { Class c }
| DOT { Any }
| text=BRACES { Action text  }
| LT e=expression GT { Capture e }
;
%%
