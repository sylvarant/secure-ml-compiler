(*
 * =====================================================================================
 *
 *       Filename:  intermediary.ml
 *
 *    Description:  Specification of the intermediary
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Modules

module Intermediary =
struct

    type CPath = string list
    type CId = string

    type Gettr = CPath * computation

    type CFile = CId * structdef list 
    and structdef = Gettr CPath 
    | Module string * modulexpr
    and modulexpr = 
    | CStruct CID * CPath * CID list
    and contents =  
    | 
    and structdef 

    and computation = TODO



    (* Print the intermediary to a c file *)
    let printfile cfile  =

        (* convert the contents *)
        let printcontents ls = ""

        (* include miniml settings from lib/ *)
        let header = ["// Compiled by lanren"; "#include \"miniml.h"; ""] 
        
        (* the top level *)
        match cfile with 
        | CFILE (name,ls) -> (String.concat (List.append header (printcontents ls)) '\n')

end
