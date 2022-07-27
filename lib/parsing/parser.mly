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
    let open Command in
    let open Common.Declarations in
    let term = Lambda (args, Ascription (body, ty')) in
    let ty  = Prod (args, ty') in
    let const_def = { name; ty; term } in
    Define const_def

  (* The inductive's parameters are included immediately in the constructors during parsing *)
  let mk_ind_decl ind params' sort ctors =
    let open Command in
    let open Common.Declarations in
    (* Sets anonymous ids to a default value *)
    let fix_name (x, t) = Option.value ~default:Name.default x, t in
    let params = List.map fix_name params' in
    let mk_ctor_decl ( name, args', ty') =
      let ty = Prod (params' @ args', ty') in
      let args = List.map fix_name args' in
      { name; ind; params; args; ty }
    in
    let ctor_decls = List.map mk_ctor_decl ctors in
    (* sort level is a placeholder *)
    let ind_decl = { name = ind; params; sort; level = 9999; ctors=(List.map (fun c -> c.name) ctor_decls)} in
    Inductive (ind_decl, ctor_decls)
%}

/* Token definitions 

    These are the output of the lexer rules
*/
%token <int> INT
%token <string> ID FILENAME
%token COLON COMMA ARROW BIG_ARROW VBAR AT ASSIGN
%token LPAREN RPAREN
%token KWD_UNIVERSE KWD_LAMBDA KWD_UNKNOWN KWD_UNKNOWN_T KWD_FORALL
%token KWD_LET KWD_IN
%token KWD_MATCH KWD_AS KWD_RETURN KWD_WITH KWD_END
%token VERNAC_CHECK VERNAC_EVAL VERNAC_ELABORATE VERNAC_LOAD
%token VERNAC_DEFINITION VERNAC_INDUCTIVE  
%token VERNAC_SET VERNAC_FLAG_VARIANT VERNAC_FLAG_FUEL 
%token VERNAC_VARIANT_G VERNAC_VARIANT_S VERNAC_VARIANT_N
%token VERNAC_SEPARATOR
%token EOF

/* This reduces the number of error states.
   It is useful for defining better error messages.
 */
%on_error_reduce term

/* Specify starting production */
%start program_parser term_parser command_parser flag_parser

/* Types for the result of productions */
%type <Ast.term Command.t list> program_parser
%type <Ast.term> term_parser
%type <Ast.term Command.t> command_parser
%type <Config.Flag.t> flag_parser

/* Dummy starts. Only used to reduce number of error messages. */
%start ctor_decl_parser branch_parser
%type <unit> ctor_decl_parser
%type <unit> branch_parser

%%
/* Dummy parsers. Only used to reduce number of error messages. */
ctor_decl_parser :
  ctor_decl; EOF {} 

branch_parser : 
  branch_decl; EOF {}

 /* Start grammar productions */
program_parser :
  cmds=list(sequenced_command); EOF   { cmds }

term_parser :
  t=top; EOF   { t }

command_parser : 
  cmd=sequenced_command; EOF    { cmd }
  
sequenced_command :
  cmd=command ; VERNAC_SEPARATOR   { cmd }

flag_parser : 
  flag=flag; EOF    { flag }

command :
// Eval <top>
| VERNAC_EVAL; t=top                       { Eval t }
// Check <top>
| VERNAC_CHECK; t=top                      { Check t }
// Elab <top>
| VERNAC_ELABORATE; t=top                  { Elab t }
// Set <flag>
| VERNAC_SET; flag=flag                    { Set flag }
// Definition foo (x : Type1) : Type1 := ...
| VERNAC_DEFINITION; id=id; args=args0; COLON; ty=term; ASSIGN ; body=top  
 { mk_definition id args ty body }
// Load "filename"
| VERNAC_LOAD; filename=FILENAME  { Load filename }
// Inductive list (a : Type0) : Type0 := <ctor_decls>
| VERNAC_INDUCTIVE; id=id; params=args0; COLON; ty=term; ASSIGN; ctors=list(ctor_decl)
 { mk_ind_decl id params ty ctors }

ctor_decl :
| VBAR; id=id; args=args0; COLON; ty=term { (id, args, ty) }

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
%inline args0 :
| args=list(arg) { List.flatten(args) }

top :
| t=top; COLON; ty=term                                           { Ascription (t, ty) }
| t=term                                                          { t }

term :
| KWD_LAMBDA; args=args; BIG_ARROW; body=term                      { Lambda (args, body) }
| KWD_FORALL; args=args; COMMA; body=term                          { Prod (args, body) }
| dom=fact; ARROW; body=term                                       { Prod ([(None, dom)], body) }
| KWD_LET; id=id; COLON; ty=term; ASSIGN; t1=term; KWD_IN; t2=term { LetIn (id, ty, t1, t2) }
// match@list nil as z return P with <branch_decls>
| KWD_MATCH; AT; ind=id; discr=top; KWD_AS; z=id; 
  KWD_RETURN; pred=top; KWD_WITH; branches=list(branch_decl); KWD_END
  { Match {ind; discr; z; pred; branches} }
| t=fact  
                                                        { t } 

branch_decl : 
| VBAR; ctor=id; ids=list(id); BIG_ARROW; body=top { { ctor; ids; body} }

fact :
| t=fact; u=atom                                       { App (t, u) }
| t=atom                                               { t }

atom : 
| LPAREN; t=top; RPAREN                                { t }
| id=id                                                { Var id }
| KWD_UNIVERSE; AT; i=INT                              { Universe i }
| KWD_UNIVERSE                                         { Universe 0 }
| KWD_UNKNOWN; i=INT                                   { Unknown i }
| KWD_UNKNOWN_T; i=INT                                 { UnknownT i }



