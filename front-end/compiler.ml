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

open Mini
open Modules (*opening mini should open this *)




    (* Print the intermediary to a c file *)
    let printfile env  =

        (* convert the contents *)
        let printcontents ls = "" in

        (* include miniml settings from lib/ *)
        None 



(*-----------------------------------------------------------------------------
 *  The compiler for MiniML ml
 *-----------------------------------------------------------------------------*)
module CCompiler =
struct
    open MiniML 
    open MiniMLMod


    (* Exceptions *) 
    exception Cannot_compile of string

    (* types used during compilation *)
    type cpath = string list
    and modbindtype = FB of cpath * string * modbindtype list | SB of cpath * modbindtype list
    and strctbinding = BVal of string * modbindtype | BMod of string * modbindtype 
    and computation = TODO


   (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    compile
    *  Description:    compiles MiniML into MLC, returns a list of getters and structs
    * =====================================================================================
    *)
    let compile program = 


        (* header for the compiled result *)
        let header = ["// Compiled by lanren"; "#include \"miniml.h"; ""] in

        (* look up x in the env *)
        let get_binding x env = 
            let find_binding = (function
            | BVal (nn,_) when (nn = x) -> true
            | BMod (nn,_) when (nn = x) -> true
            | _ -> false) in
            try (List.find (find_binding) env)
            with _ -> raise (Cannot_compile "Identifier not found") in

        (* convert a path into a list of strings *)
        let rec convert_path = (function Pident id -> [Ident.name id]
            | Pdot (p,str) -> str :: (convert_path p)) in

        (* convert path to string *)
        let rec path_str = function
              Pident id -> (Ident.name id) 
            | Pdot(root, field) -> ((path_str root) ^ "." ^ field) in
        
        (* convert a sequence of structure definitions *)
        let rec parse_struct env path strctls   = 

            let rec parse_computation env = function _ -> TODO in

            (* convert a module definition into a new environment *)
            let rec parse_module env pth modterm = 

                let rec lookup_path env :  = (function x::[] -> let BMod (_,e) = (get_binding x env) in e
                    | x::xs -> let BMod (_,e) = (get_binding x (lookup_path env xs) ) in e) in
            
                match modterm with Longident path -> (lookup_path env (convert_path path))
                    | Structure strls -> let parsed = (parse_struct env pth strls) in SB (path,parsed)
                    | Functor (id,ty,m) -> FB (path,id.name,m)
                    | Apply (m1,m2) -> 
                        match (parse_module env pth m1) with
                            | FB (_,id,m) -> let nenv = (parse_module env pth m2) in
                                (parse_module ((BMod (id,nenv))::env) pth m)
                            | _ -> raise (Cannot_compile "Needed Functor")
                | Constraint (m,ty) -> raise Cannot_compile "Constraint !" in

            (* recurse over the list of definitions *)
            match strctls with [] -> []
                | x::xs ->  match x with Type_str _ -> ""
                    | Module_str (id,mterm) -> 
                        let nenv = (parse_module env (id.name::path) mterm) in
                        let value = BMod (id.name,nenv) in
                        value :: (parse_struct (value :: env) path xs)
                    | Value_str (id,term) -> 
                        let comp = (parse_computation env term) in
                        let data = BVal ((id.name), (id :: path ,comp)) in 
                            data :: (parse_struct (data :: env) path xs) in
                
        (* top level *)
        match program with Structure strt -> (parse_struct [] [] strt) 
        | _ ->  
            prerr_string "Error: Not interested in this program"; 
            prerr_newline(); 
            exit 2
end


