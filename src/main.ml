(*
 * =====================================================================================
 *
 *       Filename:  main.ml
 *
 *    Description:  Compile the AST into bytecode
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 *)

(* Requires Module system, MiniML Ast, MiniML Typechecker, MiniML scoping and Compiler *)
open Modules
open Mini
open Typechecker
open Scoping
open Compiler
open Prettyprinter


let init_scope = ref Scope.empty
let init_env = ref MiniMLEnv.empty

let enter_type id decl =
  init_scope := Scope.enter_type id !init_scope;
  init_env := MiniMLEnv.add_type id decl !init_env

let enter_val name ty =
  let id = Ident.create name in
  init_scope := Scope.enter_value id !init_scope;
  init_env := MiniMLEnv.add_value id ty !init_env


(* TODO delete what is unnecessary *)
let _ =
  enter_type MiniML.ident_arrow {MiniMLMod.kind = {MiniML.arity = 2}; MiniMLMod.manifest = None};
  enter_type MiniML.ident_star {MiniMLMod.kind = {MiniML.arity = 2}; MiniMLMod.manifest = None};
  enter_type MiniML.ident_int {MiniMLMod.kind = {MiniML.arity = 0}; MiniMLMod.manifest = None};
  enter_type MiniML.ident_bool {MiniMLMod.kind = {MiniML.arity = 0}; MiniMLMod.manifest = None};
  List.iter
    (fun name ->
        enter_val name
          { MiniML.quantif = [];
            MiniML.body = MiniML.arrow_type MiniML.int_type 
                (MiniML.arrow_type MiniML.int_type MiniML.bool_type)})
    ["+"; "-"; "*"; "/"; "=="; "<>"; "<"; "<="; ">"; ">="];
  let alpha = MiniMLTyping.newvar() and beta = MiniMLTyping.newvar() in
  let talpha = MiniML.Var alpha and tbeta = MiniML.Var beta in
  enter_val ","
    { MiniML.quantif = [alpha;beta];
      MiniML.body = MiniML.arrow_type talpha (MiniML.arrow_type tbeta
                  (MiniML.Typeconstr(MiniML.path_star, [talpha; tbeta]))) };
  enter_val "fst"
    { MiniML.quantif = [alpha;beta];
      MiniML.body = MiniML.arrow_type
                  (MiniML.Typeconstr(MiniML.path_star, [talpha; tbeta])) talpha };
  enter_val "snd"
    { MiniML.quantif = [alpha;beta];
      MiniML.body = MiniML.arrow_type
                  (MiniML.Typeconstr(MiniML.path_star, [talpha; tbeta])) tbeta };
  enter_val "conditional"
    { MiniML.quantif = [alpha];
      MiniML.body =MiniML.arrow_type MiniML.bool_type
                          (MiniML.arrow_type talpha (MiniML.arrow_type talpha talpha)) }


(*
 * ===  FUNCTION  ======================================================================
 *         Name:    main
 *  Description:    feeds the lexer from stdin and then parses and compiles it
 *          TODO    compile modules
 * =====================================================================================
 *)

let main() =
  let lexbuf = Lexing.from_channel stdin in
  try
    let prog = Parser.implementation Lexer.token lexbuf in
    let scoped_prog = MiniMLModScoping.scope_module !init_scope prog in
   (* let mty = MiniMLModTyping.type_module !init_env scoped_prog in *)
    let compilation =  (CCompiler.compile scoped_prog) in
    (print_string compilation);
    exit 0
  with
    Error s ->
      prerr_string "Error: "; prerr_string s; prerr_newline(); exit 2
  | Parsing.Parse_error ->
      prerr_string "Syntax error at char ";
      prerr_int (Lexing.lexeme_start lexbuf);
      prerr_newline();
      exit 2
  | Lexer.Lexical_error msg ->
      prerr_string "Lexical error: "; prerr_string msg;
      prerr_string ", around character ";
      prerr_int (Lexing.lexeme_start lexbuf);
      prerr_newline();
      exit 2

let _ = main()
