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

open Modules
open Mini
open Typechecker
open Scoping
open Compiler
open Printer
open Intermediary


(*
 * ===  FUNCTION ======================================================================
 *         Name:  main
 *  Description:  feeds the lexer from stdin and then parses and compiles it
 * =====================================================================================
 *)
let main() =
  let lexbuf = Lexing.from_channel stdin in
  try 
    (* Step 1 : Parse *)
    let prog = Parser.implementation Lexer.token lexbuf in
    
    (* Step 2 : Scope *)
    let scoped_prog = MiniMLModScoping.scope_module Scope.empty prog in

    (* Step 3 : TypeCheck *)
    let mty = MiniMLModTyping.type_module MiniMLEnv.empty scoped_prog in
    log_type mty;

    (* Step 4 : Compile *)
    let compilation = (CCompiler.compile mty scoped_prog) in

    (* Step 5 : Output *) 
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
  | Cannot_Convert_Intermediary str -> prerr_string str;
    prerr_newline (); 
    exit 6

let _ = main()

