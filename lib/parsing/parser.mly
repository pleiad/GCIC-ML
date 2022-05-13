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
%token  <int> INT
%token  <string> ID
%token  COLON
%token  DOT
%token  LPAREN RPAREN
%token  KWD_UNIVERSE
%token  KWD_LAMBDA
%token  KWD_PROD
%token  KWD_UNKNOWN
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

term :
| KWD_LAMBDA; id=ID; COLON; ty=term; DOT; body=term    { Lambda (Name.of_string id, ty, body) }
| KWD_PROD; id=ID; COLON; ty=term; DOT; body=term      { Prod (Name.of_string id, ty, body) }
| t=fact                                               { t } 

fact :
| t=fact; u=atom                                       { App (t, u) }
| t=atom                                               { t }

atom : 
| LPAREN; t=term ; RPAREN                              { t }
| id=ID                                                { Var (Name.of_string id) }
| KWD_UNIVERSE; i=INT                                  { Universe i }
| KWD_UNKNOWN; i=INT                                   { Unknown i }



