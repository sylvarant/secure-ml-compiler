(*
 * =====================================================================================
 *
 *     Filename:  modules_compiler.ml
 *
 *  Description:  Compile the Modules into our own representation
 *
 *     Author:  Adriaan Larmuseau, ajhl
 *    Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Mini
open Modules 
open Intermediary

(* Exceptions *) 
exception Omega_miss of string
exception Cannot_compile_module of string
exception Cannot_compile of string


(*-----------------------------------------------------------------------------
 *  General Compiler type
 *-----------------------------------------------------------------------------*)
module type CCOMPILER =
sig
    type compilertype = MiniMLMod.mod_type -> MiniMLMod.mod_term -> string -> (string * string)
    val compile : compilertype
end

(* 
 * ===  MODULE  ======================================================================
 *         Name:  Omega
 *  Description:  The modules representation
 * =====================================================================================
 *)
module Omega(Intermediary : CINTERMEDIARY) =
struct

 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type cpath = string list

  and modbindtype = FB of cpath * string * MiniMLMod.mod_term * modbindtype * bool
                  | SB of cpath * strctbinding list * bool
                  | AR of string * string list option

  and strctbinding = BVal of string * cpath * computation 
                   | BMod of string * modbindtype 
                   | BArg of string  (* functor argument *)

  and computation = Intermediary.tempc list * Intermediary.tempc list * Intermediary.tempc 

  (* the results of look ups into the omega environement *)
  and trawl = Static of string 
            | Environment of modbindtype
            | Dynamic of string * string list option

  and omega = strctbinding list

  type compred = Gettr of string * Intermediary.type_u * Intermediary.locality * computation 
               | Strct of cpath 
               | Fctr of  string * Intermediary.locality 
               | Compttr of string * computation * Intermediary.tempc list


 (*-----------------------------------------------------------------------------
  *  Helper Functions
  *-----------------------------------------------------------------------------*)

  (* convert a path into a list of strings *)
  let rec convert_path = (function Pident id -> [Ident.name id]
    | Pdot (p,str) -> str :: (convert_path p)) 
  
  (* convert a list into a pointer *)
  let make_entrypoint = function [] -> "toplevel" 
    | lst -> (String.concat "_" (List.rev lst)) 

  let make_ptr lst = "_" ^ (make_entrypoint lst)

  (* the difference between a ptr and a path is the path is the . *)
  let make_path = function [] -> raise (Omega_miss "Not a path")
    | lst  -> (String.concat "." (List.rev lst))

  (* get_static *)
  let rec get_static = function
    | Static str -> str
    | _ -> raise (Omega_miss "Environment instead of Static")


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  lookup_path
  *  Description: Fetches the environent of list of paths
  * =====================================================================================
  *)
  let rec lookup_path env path = 

    (* note that this is not the original one *)
    let set_origin = function 
      | FB (a,b,c,d,_) -> FB(a,b,c,d,false)
      | SB (a,b,_) -> SB(a,b,false)
      | AR _  as a -> a
    in

    (* look up x in the env *)
    let get_binding x nenv = 
      let find_binding = (function
      | BVal (nn,_,_) when (nn = x) -> true
      | BMod (nn,_) when (nn = x) -> true
      | BArg nn when (nn = x) -> true
      | _ ->  false) in
      try (List.find (find_binding) nenv)
      with Not_found -> raise (Omega_miss "Identifier not found") 
    in

    (* When it comes to values all that matters is that they exist *)
    let extract = function
        | BVal (nn,pth,_) -> Static (make_ptr (nn::pth))
        | BMod (_,e) -> (Environment (set_origin e)) 
        | BArg nn -> (Dynamic (nn,None))
    in

    (* toplevel *)
    match path with  | [] -> raise (Omega_miss "Empty path given to lookup")
      | x::[] -> (extract (get_binding x env)) 
      | x::xs as ls -> match (lookup_path env xs) with
        | Environment( SB (_,nenv,_)) -> (lookup_path nenv (x::[]))
        | Dynamic (nn,None) -> Dynamic (nn, Some (List.rev (List.tl (List.rev ls))))
        | _ -> raise (Cannot_compile "Wrong tree structure") 

end


(*-----------------------------------------------------------------------------
 *  Expression Compiler type
 *-----------------------------------------------------------------------------*)
module type EXPR_COMP =
sig
  module Intermediary : CINTERMEDIARY
  type exprcomp = Omega(Intermediary).compred list * Omega(Intermediary).computation
  val compile : Omega(Intermediary).omega -> Omega(Intermediary).cpath ->  MiniML.term -> exprcomp
end


(* 
 * ===  MODULE  ======================================================================
 *         Name:  ModComp
 *  Description:  The static modules compiler
 * =====================================================================================
 *)
module ModComp(ExprComp : EXPR_COMP) =
struct
  
  module Interm = ExprComp.Intermediary
  module MOmega = Omega(ExprComp.Intermediary)
  open MOmega 
  open MiniMLMod


 (*-----------------------------------------------------------------------------
  *  High Level Modules Compiler
  *-----------------------------------------------------------------------------*)
  module High =
  struct

   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  compile
    *  Description:  converts the toplevel into an omega binding
    * =====================================================================================
    *)
    let compile program = 

      let functlist = ref [] in

      (* convert a sequence of structure definitions *)
      let rec parse_struct env path strctls  = 

        (* convert a module definition into a new environment *)
        let rec parse_module env pth = function
            Longident ident -> (match (lookup_path env (convert_path ident)) with 
              | (Environment e) -> e
              | Dynamic (n,ls) -> AR (n,ls)
              | _ -> raise (Cannot_compile_module "Did not retrieve environment from path lookup"))
          | Structure strls -> let parsed = (parse_struct env pth strls) in SB (pth,parsed,true)
          | Functor (id,ty,m) -> let bind = BArg (Ident.name id) in
            let mb = (parse_module (bind::env) ("Functor"::pth) m) in (* compile functor cont with an arg *)
              FB (pth,(Ident.name id),m,mb,true)
          | Apply (m1,m2) -> 
            (match (parse_module env pth m1) with
              | FB (_,id,m,_,_) -> let nenv = (parse_module env pth m2) in
                (parse_module ((BMod (id,nenv))::env) pth m)
              | _ -> raise (Cannot_compile_module "Needed Functor"))
          | Constraint (m,ty) -> (parse_module env pth m) 
        in

        (* recurse over the list of definitions *)
        match strctls with [] -> []
          | x::xs ->  match x with 
              Type_str (id2,_,_) -> (parse_struct env path xs)
            | Module_str (id2,mterm) ->
              let name = (Ident.name id2) in
              let nenv = (parse_module env (name::path) mterm) in
              let value = BMod (name,nenv) in
              value :: (parse_struct (value :: env) path xs)
            | Value_str (id2,term) ->
              let name = (Ident.name id2) in
              let (flist,comp) = (ExprComp.compile env (name::path) term) in 
              functlist := flist @ !functlist;
              let data = BVal (name, path, (comp)) in 
                data :: (parse_struct (data :: env) path xs) 
      in
        
      (* top level *)
      match program with Structure strt -> (!functlist,(parse_struct [] [] strt))
      | _ ->  raise (Cannot_compile_module "Top level must be structure")


   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  extract
    *  Description: extract the compiler redices from the omega structure produced by st*
    * =====================================================================================
    *)
    let rec extract path = function [] -> ([],[],[]) 
      | str::ls -> (match str with
        | BArg _ -> raise (Cannot_compile_module "Cannot extract BArg")
        | BVal (name, _, comp) -> let ptr = make_ptr (name::path) 
          and (a,b,c) = (extract path ls) in
          ((Gettr (ptr,(Interm.constd Interm.VALUE),Interm.ENTRYPOINT,comp)) :: a, b, c)
        | BMod (name, modt) -> (match modt with
          | AR _ -> raise (Cannot_compile_module "Cannot extract AR")
          | SB (pth,nbinding,true) ->  let (aa,bb,cc) = (extract (name::path) nbinding)  
            and (a,b,c) = (extract path ls) in
            ( aa @ a, (Strct pth) :: bb @ b, cc @ c) 
          | SB (pth,nbinding,false) -> (extract path ls)
          | FB (pth,var,mm,mb,_) -> let npth = (make_ptr pth) 
            and (a,b,c) =  (extract path ls) in
            (a, b, (Fctr (npth,Interm.ENTRYPOINT)) :: c)))
  end


 (*-----------------------------------------------------------------------------
  *  Low Level Modules Compiler (To the intermediary)
  *-----------------------------------------------------------------------------*)
  module Low =
  struct

    open Interm

   (*-----------------------------------------------------------------------------
    *  Helper Funcions
    *-----------------------------------------------------------------------------*)

    (* print the tuple computation *)
    let rec computation = function (vlist,sls,comp) -> 
      let c = [("return "^(printc comp))] in
      let l = match sls with [] -> []
        | _ -> (List.map printc sls)
      in
      let v = match vlist with [] -> []
        | _ -> [(printd VALUE)^ " " ^(String.concat "," (List.map printc vlist))] 
      in
        v@l@c 

    (* build funcdefi *)
    let funcdef b = function
      | Gettr (ptr,dtstr,loc,comp) -> (loc,dtstr,ptr,[],b)
      | Fctr (ptr,loc) -> (loc,(constd VOID),ptr,[],b)
      | _ -> raise (Cannot_convert_intermediary "funcdef failed")


   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  lambda
    *  Description: print a lambda function implementation
    * =====================================================================================
    *)
    let rec lambda = function [] -> []
      | Compttr (name,comp,setup) :: xs -> 
          let args = [(BINDING,MOD);(BINDING,ENV);(VALUE,ARG)] in
          let definition = (LOCAL,(constd VALUE),name,args,false) in
          let setupls : string list = (List.map printc setup)  in
          let body = (format 1 (setupls @ (computation comp))) in
          (String.concat "\n" (((printf definition)::body) @ func_end)) :: (lambda xs) 
      | _ -> raise (Cannot_convert_intermediary "print_lambdas - only compiles Compttr")

   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  getter
    *  Description: print a getter
    * =====================================================================================
    *)
    let rec getter = function [] -> []
      | Gettr  (ptr,dtstr,loc,comp) :: xs -> 
        let definition = (loc,dtstr,ptr,[],false) in
        let env = (printd BINDING)^" "^(printconst ENV)^" = NULL" 
        and md = (printd BINDING)^" "^(printconst MOD)^"= NULL" in
        let setup = [env ; md] in
        let body = (format 1 (setup @ (computation comp))) in
        (String.concat "\n" ( ((printf definition)::body) @ func_end ) ) :: (getter xs) 
      | _ -> raise (Cannot_convert_intermediary "print_getters - only compiles Gettr")

  end

end
 
