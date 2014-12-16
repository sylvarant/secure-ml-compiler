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
open Printer
open Secure_compiler
open Modules_compiler
open Intermediary

(*-----------------------------------------------------------------------------
 *  Exceptions
 *-----------------------------------------------------------------------------*)
 
exception CommandlineArg 


(*-----------------------------------------------------------------------------
 *  Types
 *-----------------------------------------------------------------------------*)

type inputspec = { 
  mutable compiler : SecCompiler.compilertype; 
  mutable obj : string; 
  mutable header : string
}


(*-----------------------------------------------------------------------------
 *  Global vars
 *-----------------------------------------------------------------------------*)

let comptype_enum = [SecCompiler.compile]


(*
 * ===  FUNCTION ======================================================================
 *         Name:  commandline
 *  Description:  process the command line arguments
 * =====================================================================================
 *)
let commandline = 
  let default = { compiler = SecCompiler.compile; obj = "object.c" ; header = "header.h"} in
  let rec process = function
    | x :: y :: ls -> if x = "-t" 
      then (default.compiler <- (List.nth comptype_enum (int_of_string y));
        process ls)
      else if x = "-o"
      then (default.obj <- y;
        process ls)
      else if x = "-h"
      then (default.header <- y;
        process ls)
      else ()
    | _ -> ()
  in
  (process (List.tl (Array.to_list Sys.argv)));
  default

(*
 * ===  FUNCTION ======================================================================
 *         Name:  main
 *  Description:  feeds the lexer from stdin and then parses and compiles it
 * =====================================================================================
 *)
let main() =
  let spec = commandline in
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
    let (obj,header) = (spec.compiler mty scoped_prog (Filename.basename spec.header)) in

    (* Step 5 : Output *) 
    let obj_chan = open_out spec.obj in
    let header_chan = open_out spec.header in
    (output_string obj_chan obj);
    (output_string header_chan header);
    exit 0

  with Error s -> prerr_string "Error: "; 
    prerr_string s; 
    prerr_newline(); 
    exit 1
  | Expression_compiler.Cannot_compile s -> prerr_string s; 
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
  | Cannot_convert_intermediary str -> prerr_string str;
    prerr_newline (); 
    exit 6

let _ = main()

