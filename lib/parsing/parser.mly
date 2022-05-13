/* This is the specification for the parser */
// TODO: Manage applications better, probably with a stratified grammar approach or directives?

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
%token COLON DOT ARROW
%token LPAREN RPAREN
%token KWD_UNIVERSE KWD_LAMBDA KWD_UNKNOWN
%token EOF

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
| id=ID { Name.of_string id }

prod_arg :
(* (x : A) -> B *)
| LPAREN; id=id; COLON; dom=term; RPAREN                  { (id, dom) }
(* A -> B *)
| dom=term                                             { (Name.of_string "_", dom) }

term :
| KWD_LAMBDA; id=id; COLON; ty=term; DOT; body=term    { Lambda (id, ty, body) }
| arg=prod_arg; ARROW; body=term                       { Prod (fst arg, snd arg, body) }
| t=fact                                               { t } 

fact :
| t=fact; u=atom                                       { App (t, u) }
| t=atom                                               { t }

atom : 
| LPAREN; t=term ; RPAREN                              { t }
| id=ID                                                { Var (Name.of_string id) }
| KWD_UNIVERSE; i=INT                                  { Universe i }
| KWD_UNKNOWN; i=INT                                   { Unknown i }



