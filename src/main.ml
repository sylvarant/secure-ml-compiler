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
open MiniMLMod
open Typechecker
open Scoping
open Printer
open Secure_compiler
open Normal_compiler
open Modules_compiler
open Intermediary

(*-----------------------------------------------------------------------------
 *  Exceptions
 *-----------------------------------------------------------------------------*)
 
exception CommandlineArg of string


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

let comptype_enum = [SecCompiler.compile;NormCompiler.compile]


(*
 * ===  FUNCTION ======================================================================
 *         Name:  commandline
 *  Description:  process the command line arguments
 * =====================================================================================
 *)
let commandline = 
  let default = {compiler = SecCompiler.compile; obj = "object.c" ; header = "header.h"} in
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
  let arguments = List.rev (List.tl (Array.to_list Sys.argv)) in
  let file = (List.hd arguments) in
  try
    let options = List.rev (List.tl arguments) in
    (process options);
    (file,default)
  with _ -> raise (CommandlineArg "Incorrect Arguments")


(*
 * ===  FUNCTION ======================================================================
 *         Name:  preprocess
 *  Description:  preprocessor for the open statements
 * =====================================================================================
 *)
let rec preprocess prog dirname = 

  (* look up the module *)
  let fetch_module id = 
    let fail arg = (Printf.eprintf "Failed opening Module %s\n" arg) in
    let inputf = Filename.concat dirname ((String.uncapitalize id)^".ml") in
    (Printf.eprintf "%s\n" inputf);
    let channel = (Pervasives.open_in inputf) in
    let lexbuf = Lexing.from_channel channel in
    try
      let m = Parser.implementation Lexer.token lexbuf in
      let pm = preprocess m dirname in
      let sm = MiniMLModScoping.scope_module Scope.empty pm in
      let mty = MiniMLModTyping.type_module MiniMLEnv.empty sm in 
      (* only return the prog for now *)
      sm 
    with
      | Error s -> (fail id); 
        (Printf.eprintf "Error: %s\n" s);
        exit 7
      | Cannot_TypeCheck ty -> (fail id); 
        (Printf.eprintf "Type Error: %s\n" (string_typefail ty));
        exit 8
  in

  (* do preprocessing *)
  let rec process_module = function
    | Longident path as x -> x
    | Structure str -> Structure (process_struct str)
    | Functor(id, arg, body) -> Functor (id,arg,(process_module body))
    | Apply(m1, m2) -> Apply ((process_module m1),(process_module m2))
    | Constraint(m, mty) -> Constraint ((process_module m),mty)

  and process_struct = function [] -> []
    | x :: xs -> match x with
      | Open_str(id) -> let m = (fetch_module (Ident.name id)) in 
          Module_str(id,m) :: (process_struct xs)
      | Module_str(id, m) -> Module_str(id, (process_module m)) :: (process_struct xs)
      | _ -> x :: (process_struct xs)
  in

  (* top level *)
  (process_module prog)


(*
 * ===  FUNCTION ======================================================================
 *         Name:  main
 *  Description:  feeds the lexer from stdin and then parses and compiles it
 * =====================================================================================
 *)
let main() =
  let (file,spec) = commandline in
  let input = Pervasives.open_in file in
  let lexbuf = Lexing.from_channel input in
  try 
    (* Step 1 : Parse *)
    let prog = Parser.implementation Lexer.token lexbuf in

    (* Step 2 : PreProcess *)
    let processed = preprocess prog (Filename.dirname file) in

    (* Step 3 : Scope *)
    let scoped_prog = MiniMLModScoping.scope_module Scope.empty processed in

    (* Step 4 : TypeCheck *)
    let mty = MiniMLModTyping.type_module MiniMLEnv.empty scoped_prog in
    log_type mty;

    (* Step 5 : Compile *)
    let (obj,header) = (spec.compiler mty scoped_prog (Filename.basename spec.header)) in

    (* Step 6 : Output *) 
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
  | CommandlineArg str -> Printf.eprintf "%s\n" str;
    exit 9

let _ = main()

