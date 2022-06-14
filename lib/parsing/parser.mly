/* This is the specification for the parser 

   http://cambium.inria.fr/~fpottier/menhir/manual.html
*/

%{
  [@@@coverage exclude_file]
  open Ast
  open Common.Id
  open Config.Variant
  open Config.Flag

  (** Stores a global declaration (def) as a lambda. 
      The body of the lambda is being ascribed to the expected type.
  *)
  let mk_definition name args ty' body =
    let open Vernac.Command in
    let term = Lambda (args, Ascription (body, ty')) in
    let ty  = Prod (args, ty') in
    let const_def = Constant_def { name; ty; term } in
    Define const_def
%}

/* Token definitions 

    These are the output of the lexer rules
*/
%token <int> INT
%token <string> ID
%token <string> FILENAME
%token COLON DOT COMMA ARROW EQUAL // DOUBLE_QUOTE
%token LPAREN RPAREN
%token KWD_UNIVERSE KWD_LAMBDA KWD_UNKNOWN KWD_UNKNOWN_T KWD_FORALL
%token KWD_LET KWD_IN
%token VERNAC_CHECK VERNAC_EVAL VERNAC_ELABORATE VERNAC_DEFINITION VERNAC_SET 
%token VERNAC_FLAG_VARIANT VERNAC_FLAG_FUEL 
%token VERNAC_VARIANT_G VERNAC_VARIANT_S VERNAC_VARIANT_N
%token VERNAC_LOAD VERNAC_SEPARATOR
%token EOF

/* This reduces the number of error states.
   It is useful for defining better error messages.
 */
%on_error_reduce term

/* Specify starting production */
%start program_parser term_parser command_parser flag_parser

/* Types for the result of productions */
%type <Ast.term Vernac.Command.t list> program_parser
%type <Ast.term> term_parser
%type <Ast.term Vernac.Command.t> command_parser
%type <Config.Flag.t> flag_parser

%% /* Start grammar productions */
program_parser :
  cmds=list(sequenced_command); EOF   { cmds }

term_parser :
  t=top; EOF   { t }

command_parser : 
  cmd=sequenced_command; EOF    { cmd }
  
sequenced_command :
| cmd=command ; VERNAC_SEPARATOR   { cmd }

flag_parser : 
  flag=flag; EOF    { flag }

command :
// eval <top>
| VERNAC_EVAL; t=top                       { Eval t }
// check <top>
| VERNAC_CHECK; t=top                      { Check t }
// elab <top>
| VERNAC_ELABORATE; t=top                  { Elab t }
// set <flag>
| VERNAC_SET; flag=flag                    { Set flag }
// def foo (x : Type1) : Type1 = ...
| VERNAC_DEFINITION; id=id; args=list(arg); COLON; ty=term; EQUAL ; body=top  
 { mk_definition id (List.flatten args) ty body }
// load "filename"
| VERNAC_LOAD; filename=FILENAME  { Load filename }
(* | VERNAC_LOAD; DOUBLE_QUOTE; filename=ID; DOUBLE_QUOTE  { Load filename } *)

flag :
| VERNAC_FLAG_VARIANT; var=variant { Variant var }
| VERNAC_FLAG_FUEL; i=INT          { Fuel i }

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



