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

    type CFile = string * contents list 
    and contents =  Gettr string * computation
    |  

end
