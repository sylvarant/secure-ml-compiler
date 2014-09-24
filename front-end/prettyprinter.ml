(*
 * =====================================================================================
 *
 *       Filename:  prettyprinter.ml
 *
 *    Description:  Pretty print the types
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open MiniML
open Typechecker

module PrettyPrinter : sig

    val print_type : MiniML.simple_type -> string

end =
struct
    
    open MiniML

    (* Exceptions *) 
    exception Cannot_print of string
    
    (* print a type *)
    let print_type ty = raise Cannot_print "Function not implemented"

end

