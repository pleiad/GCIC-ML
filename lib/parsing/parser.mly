/* This is the specification for the parser 

   http://cambium.inria.fr/~fpottier/menhir/manual.html
*/

%{
  [@@@coverage exclude_file]
  open Ast
  open Common.Id
%}

/* Token definitions 

    These are the output of the lexer rules
*/
%token <int> INT
%token <string> ID
%token COLON DOT COMMA ARROW EQUAL
%token LPAREN RPAREN
%token KWD_UNIVERSE KWD_LAMBDA KWD_UNKNOWN KWD_FORALL
%token KWD_LET KWD_IN
%token VERNAC_CHECK VERNAC_EVAL VERNAC_ELABORATE VERNAC_SEPARATOR
%token EOF

/* This reduces the number of error states.
   It is useful for defining better error messages.
 */
%on_error_reduce term 

/* Specify starting production */
%start program_parser term_parser

/* Types for the result of productions */
%type <Ast.command> program_parser
%type <Ast.term> term_parser

%% /* Start grammar productions */
program_parser :
  cmd=command; EOF   { cmd }

term_parser :
  t=term; EOF   { t }

command :
| VERNAC_EVAL;t=term; VERNAC_SEPARATOR                       { Eval t }
| VERNAC_CHECK; t=term; COLON; ty=term; VERNAC_SEPARATOR     { Check (t, ty) }
| VERNAC_ELABORATE; t=term; VERNAC_SEPARATOR                 { Elab t }

id :
| x=ID { Name.of_string x }

arg :
(* (x y z : A) *)
| LPAREN; ids=nonempty_list(id); COLON; dom=term; RPAREN  { List.map (fun id -> (Some id, dom)) ids }

%inline args :
| args=nonempty_list(arg) { List.flatten(args) }

term :
| KWD_LAMBDA; args=args; DOT; body=term                           { Lambda (args, body) }
| KWD_FORALL; args=args; COMMA; body=term                         { Prod (args, body) }
| dom=fact; ARROW; body=term                                      { Prod ([(None, dom)], body) }
| KWD_LET; id=id; COLON; ty=term; EQUAL; t1=term; KWD_IN; t2=term { LetIn (id, ty, t1, t2) }
| t=fact                                                          { t } 

fact :
| t=fact; u=atom                                       { App (t, u) }
| t=atom                                               { t }

atom : 
| LPAREN; t=term; RPAREN                               { t }
| id=id                                                { Var id }
| KWD_UNIVERSE; i=INT                                  { Universe i }
| KWD_UNKNOWN; i=INT                                   { Unknown i }



