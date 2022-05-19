/* This is the specification for the parser 

   http://cambium.inria.fr/~fpottier/menhir/manual.html
*/

%{
  [@@@coverage exclude_file]
  open Ast
  open Common.Id
  open Kernel.Variant
%}

/* Token definitions 

    These are the output of the lexer rules
*/
%token <int> INT
%token <string> ID
%token COLON DOT COMMA ARROW EQUAL
%token LPAREN RPAREN
%token KWD_UNIVERSE KWD_LAMBDA KWD_UNKNOWN KWD_UNKNOWN_T KWD_FORALL
%token KWD_LET KWD_IN
%token VERNAC_CHECK VERNAC_EVAL VERNAC_ELABORATE VERNAC_SEPARATOR VERNAC_SET 
%token VERNAC_VARIANT VERNAC_VARIANT_G VERNAC_VARIANT_S VERNAC_VARIANT_N
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
  cmd=command; VERNAC_SEPARATOR; EOF   { cmd }

term_parser :
  t=top; EOF   { t }

command :
| VERNAC_EVAL; t=top                       { Eval t }
| VERNAC_CHECK; t=top                      { Check t }
| VERNAC_ELABORATE; t=top                  { Elab t }
| VERNAC_SET; VERNAC_VARIANT ; var=variant { SetVariant var }

variant : 
| VERNAC_VARIANT_G        { G }
| VERNAC_VARIANT_N        { N }
| VERNAC_VARIANT_S        { S }

id :
| x=ID { Name.of_string x }

arg :
(* (x y z : A) *)
| LPAREN; ids=nonempty_list(id); COLON; dom=term; RPAREN  { List.map (fun id -> (Some id, dom)) ids }

%inline args :
| args=nonempty_list(arg) { List.flatten(args) }

top :
| t=top; COLON; ty=term                                           { Ascription (t, ty) }
| t=term                                                          { t }

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
| LPAREN; t=top; RPAREN                                { t }
| id=id                                                { Var id }
| KWD_UNIVERSE; i=INT                                  { Universe i }
| KWD_UNKNOWN; i=INT                                   { Unknown i }
| KWD_UNKNOWN_T; i=INT                                 { UnknownT i }



