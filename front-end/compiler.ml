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

    val compile : MiniMLEnv.binding Modules.Ident.tbl -> MiniML.term list -> string

end = 
struct
    open MiniML 


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
    let compile env terms = "Compiler TODO"

        (* Typececk original program *)
        (*let types = (check_program env terms)*)
         
        (* normalize *)
        (*and anf_terms = (ANFNormalizer.normalize terms) in*)

        (* Typecheck normal form again*)
        (*let anf_types =  (check_program env anf_terms) in*)

        (* compare the results *)
        (*let _ =  (compare_types env types anf_types) in*)
    
        
end

(* 30 mins overview, how many papers & acceptance rate & picture of chair, big picture, still odd what supposed to be in there *) 


