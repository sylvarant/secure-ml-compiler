(*
 * =====================================================================================
 *
 *       Filename:  main.ml
 *
 *    Description:  Boot up the parser feed typechecker, scoper, compiler
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
open Printer


(*
 * ===  FUNCTION ======================================================================
 *         Name:  main
 *  Description:  feeds the lexer from stdin and then parses and compiles it
 * =====================================================================================
 *)
let main() =
  let init_scope = ref Scope.empty in
  let init_env = ref MiniMLEnv.empty in
  let lexbuf = Lexing.from_channel stdin in
  try (*TODO typing disabled ! *)
    let prog = Parser.implementation Lexer.token lexbuf in
    let scoped_prog = MiniMLModScoping.scope_module !init_scope prog in
    let mty = MiniMLModTyping.type_module !init_env scoped_prog in
    (*Pretty.print_modtype mty; *)
    let compilation = (CCompiler.compile mty scoped_prog) in
    (print_string compilation);
    exit 0
  with Error s -> prerr_string "Error: "; 
    prerr_string s; 
    prerr_newline(); 
    exit 1
  | Cannot_compile s -> prerr_string s; 
    prerr_newline (); 
    exit 2
  | Parsing.Parse_error -> prerr_string "Syntax error at char ";
      prerr_int (Lexing.lexeme_start lexbuf);
      prerr_newline();
      exit 3
  | Lexer.Lexical_error msg -> prerr_string "Lexical error: "; 
      prerr_string msg;
      prerr_string ", around character ";
      prerr_int (Lexing.lexeme_start lexbuf);
      prerr_newline();
      exit 4
  | Cannot_TypeCheck ty -> prerr_string (string_typefail ty); 
    prerr_newline(); 
    exit 5

let _ = main()

