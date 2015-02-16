(*
 * =====================================================================================
 *
 *     Filename: printer.ml
 *
 *  Description: Pretty print the types for debugging purposes
 *
 *       Author: MYSTERY MAN, 
 *      Company: SOMEWHERE IT
 *
 * =====================================================================================
 *)

open Modules
open Mini

(* Exceptions *) 
exception Cannot_Print of string

module Pretty  =
struct

  module Mod = MiniMLMod
  open MiniML
  open MiniMLMod
  open Format

 (*-----------------------------------------------------------------------------
  *  Helper functions
  *-----------------------------------------------------------------------------*)

  let variable_names = ref ([] : (type_variable * string) list)

  let reset_names () = variable_names := []

  let rec print_path = function
      Pident id ->
        print_string (Ident.name id)
    | Pdot(root, field) ->
        print_path root; print_string "."; print_string field

 (* 
  * ===  FUNCTION  ======================================================================
  *         Name:  print_simple_type
  *  Description:  print a simple type
  * =====================================================================================
  *)
  let rec print_simple_type ty =
    match typerepr ty with
      Var v ->
        let name =
          try
            List.assq v !variable_names
          with Not_found ->
            let n = List.length !variable_names + 1 in
            let s = String.make 1 (Char.chr(97 + n)) in
            variable_names := (v, s) :: !variable_names;
            s in print_string "'"; print_string name
    | LambdaType(TBool,_) -> print_string "Bool"
    | LambdaType(TInt,_) -> print_string "Int"
    | LambdaType(TUnit,_) -> print_string "Unit"
    | LambdaType(TRef,[t1]) -> print_string "(Ref ";
      print_simple_type t1;
      print_string ")"
    | LambdaType(TArrow,[t1;t2]) -> print_simple_type t1; 
      print_string " -> ";
      print_simple_type t2
    | LambdaType(TPair,[t1;t2]) -> print_string "("; 
      print_simple_type t1;
      print_string ",";
      print_simple_type t2;
      print_string ") " 
    | LambdaType(TIgnore,_) -> print_string "Ignore"
    | Typeconstr(path, []) ->
        print_path path
    | Typeconstr(path, [t]) ->
        print_simple_type t; print_string " "; print_path path
    | Typeconstr(path, t1::tl) ->
        print_string "(";
        print_simple_type t1;
        List.iter (fun t -> print_string ", "; print_simple_type t) tl;
        print_string ") "; print_path path
    | _ -> raise (Cannot_Print "Unkown simple type !")


 (* 
  * ===  FUNCTION  ======================================================================
  *         Name:  print_modtype
  *  Description:  print a module type
  * =====================================================================================
  *)
  let rec print_modtype = function
      Signature sg ->
        open_hvbox 2;
        print_string "(sig";
        List.iter
          (fun item -> print_space(); print_signature_item item) sg;
        print_break 1 (-2);
        print_string "end)";
        close_box()
    | Functor_type(param, arg, body) ->
        open_hvbox 2;
        print_string "functor("; print_string(Ident.name param);
        print_string ": "; print_modtype arg; print_string ")";
        print_space(); print_modtype body;
        close_box()

 (* 
  * ===  FUNCTION  ======================================================================
  *         Name:  print_signature_item
  *  Description:  print a member of the signature
  * =====================================================================================
  *)
  and print_signature_item sign = 
    let print_typedecl id decl = match decl.manifest with
      | None -> reset_names();
        print_simple_type ((path_to_simple (Pident id) decl.kind).defbody)
      | Some dty -> print_deftype id dty
    in
    let print_valtype vty = reset_names(); 
      print_simple_type vty.body
    in
    match sign with
        Value_sig(id, vty) -> open_hvbox 2;
        print_string "val "; print_string(Ident.name id);
        print_string ":"; print_space(); print_valtype vty; print_string ";";
        close_box()
      | Type_sig(id, decl) ->
        open_hvbox 2;
        print_string "type "; print_typedecl id decl;
        close_box()
      | Module_sig(id, mty) ->
        open_hvbox 2;
        print_string "module "; print_string(Ident.name id);
        print_string ":"; print_space(); print_modtype mty;
        close_box()

 (* 
  * ===  FUNCTION  ======================================================================
  *         Name:  print_typedecl
  *  Description:  print the type declaration
  * =====================================================================================
  *)
  and print_deftype id dty = reset_names();
    print_simple_type (Typeconstr(Pident id, List.map (fun v -> Var v) dty.params));
    print_string " ="; print_space(); 
    print_simple_type dty.defbody


 (* 
  * ===  FUNCTION  ======================================================================
  *         Name:  print_expression
  *  Description:  print the expressions
  * =====================================================================================
  *)
  let rec print_expression = function
    | Constant i -> print_int i           
    | Boolean b -> print_string (match b with | true -> "true" | _ -> "false")         
    | Longident p -> print_path p           
    | Pair (e1,e2) -> print_string "("; 
      (print_expression e1); print_string ",";
      (print_expression e2); print_string")"
    | Function (id,ty,e) -> print_string "(fun "; print_string (Ident.name id);
      print_string " : "; print_simple_type ty;
      print_string " = "; print_expression e; print_string ")"
    | Apply (e1,e2) -> print_string "("; print_expression e1; print_string " ";
      print_expression e2; print_string ")"
    | If (e1,e2,e3) -> print_string "(if "; print_expression e1; print_string "then "; 
      print_expression e2; print_string " else "; print_expression e3; print_string ")"
    | Let (x,e1,e2) -> print_string ("(let "^(Ident.name x)^" = "); print_expression e1; 
      print_string " in "; print_expression e2; print_string ")"
    | Prim (x,ls) -> print_string ("("^x); 
      (List.iter (fun e -> print_string " "; print_expression e) ls); 
      print_string ")"
    | Fst e -> print_string "fst "; print_expression e
    | Snd e -> print_string "snd "; print_expression e
    | Exit e -> print_string "exit "; print_expression e
    | Sequence (e1,e2) -> print_expression e1; 
      print_string ";" ;  print_space(); print_expression e2 
    | Ref e -> print_string "ref "; print_expression e
    | Deref e -> print_string "!"; print_expression e
    | Assign (e1,e2) -> print_expression e1; print_string ":="; print_expression e2
    | Letrec(x,ty,e1,e2) -> print_string ("letrec "^(Ident.name x)^" : "); 
      print_simple_type ty; print_string " = "; print_expression e1; 
      print_string " in "; print_expression e2
    | Unit -> print_string "unit"


 (* 
  * ===  FUNCTION  ======================================================================
  *         Name:  print_module
  *  Description:  print a module term
  * =====================================================================================
  *)
  let rec print_module = function
    | Longident p -> print_path p
    | Structure ls -> open_hvbox 2; print_string "struct";
      List.iter (fun item -> print_space(); print_structure item) ls;
      print_break 1 (-2); print_string "end";
      close_box()
    | Functor (id,mty,m) -> 
      print_string "functor("; print_string(Ident.name id);
      print_string ": "; print_modtype mty; print_string ") ";
      open_hvbox 2; 
      print_module m;
      close_box()
    | Apply (m1,m2) -> print_module m1; print_string "(";
      print_module m2; print_string ")"
    | Constraint (m,mty) -> print_string "("; print_module m;
      print_string ":"; print_modtype mty; print_string ")"

 (* 
  * ===  FUNCTION  ======================================================================
  *         Name:  print_structure
  *  Description:  print a structure item
  * =====================================================================================
  *)
  and print_structure = function
    | Value_str (id,e) -> open_hvbox 2;
      print_string "val "; print_string(Ident.name id);
      print_string " = "; open_hvbox 2; print_expression e; close_box();
      close_box()
    | Type_str (id,_,ty) -> open_hvbox 2;
      print_string "type "; print_string(Ident.name id);
      print_string " = "; print_deftype id ty;
      close_box()
    | Module_str (id,m) -> open_hvbox 2;
      print_string "module "; print_string(Ident.name id);
      print_string " = "; print_module m;
      close_box()
    | Open_str id -> open_hvbox 2; print_string "open "; 
      print_string (Ident.name id); close_box()

end


(* 
 * ===  FUNCTION  ======================================================================
 *         Name:  log_type
 *  Description:  print the full type to the log
 * =====================================================================================
 *)
 let log_type mty =
    (Format.set_formatter_out_channel Pervasives.stderr);
    Format.print_string ">>>>>>> TYPE <<<<<<<";
    Format.print_newline();
    Pretty.print_modtype mty; 
    Format.print_newline();
    Format.print_string ">>>>>>> DONE <<<<<<<";
    Format.print_newline();

