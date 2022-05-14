/* This is the specification for the parser */

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
%token EOF

%on_error_reduce term

/* Specify starting production */
%start program
/* Types for the result of productions */
// %nonassoc ID KWD_UNIVERSE KWD_LAMBDA KWD_PROD KWD_UNKNOWN LPAREN /* list ALL other tokens that start an expr */
// %nonassoc APP

%type <Ast.term> program

// %left APP
%% /* Start grammar productions */
program:
  t=term; EOF   { t }

id :
| x=ID { Name.of_string x }

arg :
(* (x y z : A) *)
| LPAREN; ids=nonempty_list(id); COLON; dom=term; RPAREN  { List.map (fun id -> (Some id, dom)) ids }

%inline args :
| args=nonempty_list(arg) { List.flatten(args) }

term :
| KWD_LAMBDA; args=args; DOT;   body=term                         { Lambda (args, body) }
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



