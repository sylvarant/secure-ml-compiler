(*
 * =====================================================================================
 *
 *       Filename:  compiler.ml
 *
 *    Description:  Compile the AST into C?
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 *)

(* Requires the MiniML specification and Leroy's path definition in modules and Typechecker *) 
open Mini
open Modules
open Typechecker

(*-----------------------------------------------------------------------------
 *  The bytecode compiler for MiniML ml
 *-----------------------------------------------------------------------------*)
module CCompiler : sig
    
    val compile : MiniMLMod.mod_term -> string

end = 
struct
    open MiniML 
    open MiniMLMod


    (* Exceptions *) 
    exception Cannot_compile of string

    (* ported from Leroy *)
    let variable_names = ref ([] : (type_variable * string) list)


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    new_address
     *  Description:    return a new address
     * =====================================================================================
     *)
    let new_address =
        let count = ref (-1) in
        fun () -> incr count; !count


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    emit
     *  Description:    add the seperator to all integers 
     * =====================================================================================
     *)
     let emit num = 
        let its i = (Printf.sprintf "%d" i) in
        ((its num) ^ "\n") 


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    path_str
     *  Description:    convert path to string
     * =====================================================================================
     *)
     let rec path_str = function
          Pident id -> (Ident.name id) 
        | Pdot(root, field) -> ((path_str root) ^ "." ^ field)

    

   (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    check_program
    *  Description:    type check a program
    * =====================================================================================
    *)
    let check_program env terms = 
        List.map (fun x -> MiniMLTyping.type_term env x) terms 


   (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    verify whether two type lists unify
    *  Description:    type check a program
    * =====================================================================================
    *)
    let compare_types env types1 types2 = 
       List.iter2 (fun x y -> MiniMLTyping.unify env x.body y.body)types1 types2


   (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    compile
    *  Description:    compile a list of expressions into a string of bytes
    * =====================================================================================
    *)
    let compile prog = 
        let rec parse_definitions = function [] ->
            | x::xs ->  match x with Type_str _ -> ""
                | Module_str (id,mterm) ->
                | Value_str (id,term) ->
                

        match prog with Structure str -> (parse_definitions str)
        | _ ->  
            prerr_string "Error: Not interested in this program"; 
            prerr_newline(); 
            exit 2
        
end


