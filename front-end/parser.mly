/*
 * =====================================================================================
 *
 *       Filename:  parser.mly
 *
 *    Description:  Parser for MiniML ML based on Leroy's parser for miniML
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

%{
open Modules
open ML
open Typechecker

let variables = ref ([] : (string * Ml.type_variable) list)

let reset_type_variables () =
  variables := []

let find_type_variable name =
  try
    List.assoc name !variables
  with Not_found ->
    let v = MiniMLTyping.newvar() in
    variables := (name, v) :: !variables;
    v

(* create list form two arguments *)
let prim_ls arg1 arg2 = arg1 :: arg2 :: []

%}

%token <string> IDENT
%token <int> INT

%token TINT
%token TBOOL

%token TRUE
%token FALSE
%token IS
%token SI
%token ARROW
%token COLON
%token COMMA
%token DOT
%token ELSE
%token END
%token EOF
%token EQUAL
%token EQUALEQUAL
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATEREQUAL
%token IF
%token IN
%token LESS
%token LESSEQUAL
%token LESSGREATER
%token LET
%token LIDENT
%token LPAREN
%token MINUS
%token MODULE
%token PLUS
%token QUOTE
%token RPAREN
%token SEMICOLON
%token SEMISEMI
%token SIG
%token SLASH
%token STAR
%token STRUCT
%token THEN
%token TYPE
%token VALUE


%right ARROW
%right COMMA
%right LESSGREATER LESS LESSEQUAL GREATER GREATEREQUAL
%right PLUS MINUS
%right STAR SLASH

/* Adriaan */
%start adriaan 
%type <ML.MiniML.term list> adriaan 

/*% IGNORED
%start implementation
%type <ML.MiniMLMod.mod_term> implementation
*/

/*% IGNORED
start phrase
%type <ML.MiniMLMod.definition> phrase
%start code
%type <ML.MiniMLMod.Core.term> code */
%%

/* Paths */

path:
    IDENT           { Pident(Ident.create $1) }
  | path DOT IDENT  { Pdot($1, $3) }
;

/* Value expressions for the core language */

valexpr:
    valexpr1                          { $1 }
/* %TODO what is this ?  | valexpr COMMA valexpr             { binop "," $1 $3 }*/
  | valexpr PLUS valexpr              { ML.Prim( "+",(prim_ls $1 $3)) }
  | valexpr MINUS valexpr             { ML.Prim( "-",(prim_ls $1 $3)) }
  | valexpr STAR valexpr              { ML.Prim( "*",(prim_ls $1 $3)) }
  | valexpr EQUALEQUAL valexpr        { ML.Prim( "=",(prim_ls $1 $3)) }
/* TODO add  | valexpr SLASH valexpr             { binop "/" $1 $3 }
  | valexpr LESSGREATER valexpr       { binop "<>" $1 $3 }
  | valexpr LESS valexpr              { binop "<" $1 $3 }
  | valexpr LESSEQUAL valexpr         { binop "<=" $1 $3 }
  | valexpr GREATER valexpr           { binop ">" $1 $3 }
  | valexpr GREATEREQUAL valexpr      { binop ">=" $1 $3 } */
/*  | FUNCTION IDENT COLON simpletype ARROW valexpr {ML.Function(Ident.create $2,$3 $5) } */
  | FUNCTION IDENT ARROW valexpr {ML.Function(Ident.create $2, $4) } 
  | LET IDENT valbind IN valexpr      { ML.Let(Ident.create $2, $3, $5) }
  | IF valexpr THEN valexpr ELSE valexpr { ML.If( $2, $4, $6) }
  | IS simpletype COLON valexpr      { ML.IS($2,$4)}
  | SI simpletype COLON valexpr      { ML.SI($2,$4)}
;
valexpr1:
    valexpr0 { $1 }
  | valexpr1 valexpr0 { ML.Apply($1, $2) }
;
valexpr0:
    path { ML.Longident($1) }
  | INT  { ML.Constant $1 }
  | TRUE { ML.Boolean true }
  | FALSE { ML.Boolean false }
  | LPAREN valexpr RPAREN { $2 }
;


valbind:
    EQUAL valexpr     { $2 }
  | IDENT valbind     { MiniML.Function(Ident.create $1, $2) }
;

/* Type expressions for the core language */

simpletype:
    QUOTE IDENT             { MiniML.Var(find_type_variable $2) }
  | simpletype ARROW simpletype { MiniML.Typeconstr(MiniML.path_arrow, [$1; $3]) }
  | simpletype STAR simpletype  { MiniML.Typeconstr(MiniML.path_star, [$1; $3]) }
  | TBOOL                   { MiniML.bool_type }
  | TINT                    { MiniML.int_type }
  | path                    { MiniML.Typeconstr($1, []) }
  | simpletype path         { MiniML.Typeconstr($2, [$1]) }
  | LPAREN simpletypelist RPAREN path { MiniML.Typeconstr($4, List.rev $2) }
;
simpletypelist:
    simpletype { [$1] }
  | simpletypelist COMMA simpletype { $3::$1 }
;

valuedecl:
    colon_begin_scheme simpletype
            { reset_type_variables(); MiniMLTyping.end_def(); MiniMLTyping.generalize $2 }
;
colon_begin_scheme: /* Hack to perform side effects before reading the type */
    COLON   { MiniMLTyping.begin_def(); reset_type_variables() }
;

/* Type definitions and declarations */

typedecl:
    typeparams IDENT        { ($2, {MiniML.arity = List.length $1}) }
;
typedef:
    typeparams IDENT EQUAL simpletype
      { reset_type_variables();
        ($2, {MiniML.arity = List.length $1}, {MiniML.params = $1; MiniML.defbody = $4}) }
;
typeparams:
    /* nothing */               { [] }
  | typeparam                   { [$1] }
  | LPAREN typeparamlist RPAREN { List.rev $2 }
;
typeparamlist:
    typeparam                       { [$1] }
  | typeparamlist COMMA typeparam   { $3 :: $1 }
;
typeparam:
    QUOTE IDENT { find_type_variable $2 }
;
typeinfo:
    typedef   { let (id, kind, def) = $1 in
                (id, {MiniMLMod.kind = kind; MiniMLMod.manifest = Some def})}
  | typedecl  { let (id, kind) = $1 in
                (id, {MiniMLMod.kind = kind; MiniMLMod.manifest = None}) }
;

/* Value expressions for the module language */

modulexpr:
    path                              { MiniMLMod.Longident $1 }
  | STRUCT structure END              { MiniMLMod.Structure(List.rev $2) }
  | FUNCTOR LPAREN IDENT COLON moduletype RPAREN modulexpr
                                      { MiniMLMod.Functor(Ident.create $3, $5, $7) }
  | modulexpr LPAREN modulexpr RPAREN { MiniMLMod.Apply($1, $3) }
  | LPAREN modulexpr RPAREN           { $2 }
  | modulexpr COLON moduletype        { MiniMLMod.Constraint($1, $3) }
/*  | valexpr                           { MiniMLMod.Expression $1 }  Adriaan */
;
structure:
    /*nothing*/                       { [] }
  | structure structure_item opt_semi { $2 :: $1 }
;
structure_item:
    VALUE IDENT valbind           { MiniMLMod.Value_str(Ident.create $2, $3) }
  | TYPE typedef                  { let (id, kind, def) = $2 in
                                    MiniMLMod.Type_str(Ident.create id, kind, def) }
  | MODULE IDENT COLON moduletype EQUAL modulexpr
                     { MiniMLMod.Module_str(Ident.create $2, MiniMLMod.Constraint($6, $4)) }
  | MODULE IDENT EQUAL modulexpr   { MiniMLMod.Module_str(Ident.create $2, $4) }
;
opt_semi:
    /* nothing */ { () }
  | SEMICOLON { () }
;

/* Type expressions for the module language */

moduletype:
    SIG signature END               { MiniMLMod.Signature(List.rev $2) }
  | FUNCTOR LPAREN IDENT COLON moduletype RPAREN moduletype
                                    { MiniMLMod.Functor_type(Ident.create $3, $5, $7) }
  | LPAREN moduletype RPAREN        { $2 }
;
signature:
    /*nothing*/                       { [] }
  | signature signature_item opt_semi { $2 :: $1 }
;
signature_item:
    VALUE IDENT valuedecl             { MiniMLMod.Value_sig(Ident.create $2, $3) }
  | TYPE typeinfo    { let (id, def) = $2 in MiniMLMod.Type_sig(Ident.create id, def) }
  | MODULE IDENT COLON moduletype     { MiniMLMod.Module_sig(Ident.create $2, $4) }
;

/* Toplevel entry point */

implementation:
   | modulexpr EOF                     { $1 }

adriaan:
    | valexpr EOF { [$1] }
    | valexpr SEMISEMI adriaan { $1 :: $3 } 
    | EOF { [] }


