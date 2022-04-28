/* This is the specification for the parser */
// TODO: Manage applications better, probably with a stratified grammar approach or directives?

%{
  [@@@coverage exclude_file]
  open Ast
%}

/* Token definitions 

    These are the output of the lexer rules
*/
%token  <int> INT
%token  <string> ID
%token  COLON
%token  DOT
%token  LBRACK RBRACK
%token  LPAREN RPAREN
%token  AT
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
| t=base_term                                          { t }
| KWD_LAMBDA; id=ID; COLON; ty=term; DOT; body=term    { Lambda(Name.of_string id, ty, body) }
| KWD_PROD; id=ID; COLON; ty=term; DOT; body=term      { Prod(Name.of_string id, ty, body) }
| t=base_term; t2=base_term                            { App(t, t2) }


base_term: 
| LPAREN; t=term ; RPAREN                           { t }
| id=ID                                             { Var(Name.of_string id) }
| KWD_UNIVERSE; i=INT                               { Universe i }
| KWD_UNKNOWN; AT ; LBRACK; i=INT; RBRACK           { Unknown i }



