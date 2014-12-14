(*
 * =====================================================================================
 *
 *     Filename:  secure_compiler.ml
 *
 *  Description:  Compile the AST into C in a fully abstract way
 *
 *     Author:  Adriaan Larmuseau, ajhl
 *    Company:  Uppsala IT
 *
 * =====================================================================================
 *)

open Mini
open Modules 
open Modules_compiler
open Expression_compiler

(*-----------------------------------------------------------------------------
 * Setup Module Compiler 
 *-----------------------------------------------------------------------------*)
module MiniModComp = ModComp(MiniMLComp)


(* ===  MODULE  ======================================================================
 *         Name:  SecCompiler
 *  Description:  The secure compiler
 * =====================================================================================
 *)
module SecCompiler : CCOMPILER =
struct
  
  module MC = MiniModComp
  open MiniML 
  open MiniMLMod
  open MC.MOmega
  open MiniMLComp.Intermediary


 (*-----------------------------------------------------------------------------
  *  Exceptions
  *-----------------------------------------------------------------------------*)
  exception Found of string list


 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type compilertype = MiniMLMod.mod_type -> MiniMLMod.mod_term -> (string * string)

  type typetrawl = Fail | SimpleType of simple_type | Modtype of mod_type 
                 | ManifestType of def_type option

  type assoc = Call of val_type * string * string list
             | Share of MiniMLMod.mod_type * string * string list
 

 (*-----------------------------------------------------------------------------
  *  Helper Funcions
  *-----------------------------------------------------------------------------*)

  (* sort signature components *)
  let sort_sigs sls =
    let cmp_sig a b = match(a,b) with
      | (Value_sig (id1,_) , Value_sig(id2,_)) -> (String.compare (Ident.name id1) (Ident.name id2))
      | (Module_sig (id1,_), Module_sig(id2,_)) -> (String.compare (Ident.name id1) (Ident.name id2))
      | (Type_sig (id1,_), Type_sig(id2,_)) -> (String.compare (Ident.name id1) (Ident.name id2))
      | (Type_sig _ , _) -> -1  
      | (Module_sig _, _) -> 1
      | (Value_sig _, Type_sig _ ) -> 1
      | (Value_sig _ , Module_sig _ ) -> -1
    in
    (List.sort cmp_sig sls)

  (* sort bindings *)
  let sort_bindings bls = 
    let cmp_binding a b = match (a,b) with
      | (BVal (name1,_,_) , BVal(name2,_,_)) -> (String.compare name1 name2)
      | (BMod (name1,_) , BMod(name2,_)) -> (String.compare name1 name2)
      | (BVal _, _) -> -1
      | (BMod _, _) -> 1
    in
    (List.sort cmp_binding bls)

  (* sort compiler redices *)
  let sort_compred cls =
    let cmp_red a b = match (a,b) with
      | (Gettr(str,_,_),Gettr(str2,_,_)) -> (String.compare str str2)
      | (Strct pth , Strct pth2) -> (String.compare (make_ptr pth) (make_ptr pth2))
      | (Fctr (str,_),Fctr (str2,_)) -> (String.compare str str2)
      | (Compttr (str,_,_),Compttr (str2,_,_)) -> (String.compare str str2)
      | (Gettr _, _)     -> 1
      | (_, Gettr _)     -> -1
      | (Compttr _, _)   -> -1
      | (_, Compttr _)   -> 1
      | (Strct _,Fctr _) -> 1
      | (Fctr _,Strct _) -> -1
    in
    (List.sort cmp_red cls)

  (* sort assoc *)
  let sort_assocs als =
    let cmp_ass a b = match (a,b) with
      | (Call (_,n,pth),Call (_,n2,pth2)) -> (String.compare (make_ptr (n::pth)) (make_ptr (n2::pth2))) 
      | (Share (_,n,pth),Share (_,n2,pth2)) -> (String.compare (make_ptr (n::pth)) (make_ptr (n2::pth2)))
      | (Call _, _)  -> 1
      | (Share _, _) -> -1
    in
    (List.sort cmp_ass als)


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name: look_up_type_path
  *  Description: find a path in the modules type decl
  * =====================================================================================
  *)
  let rec look_up_type_path ty path = 
    (* find a string in a mod_type *)
    let rec find str mty = 
      let rec find_decl = function [] -> Fail
        | x::xs -> match x with
          | Value_sig(id, vty) when (Ident.name id) = str -> SimpleType vty.body
          | Type_sig(id, decl) when (Ident.name id) = str -> ManifestType decl.manifest
          | Module_sig(id,mty) when (Ident.name id) = str -> Modtype mty
          | _ ->  (find_decl xs)
      in
      match mty with 
        | Signature sls -> (find_decl sls)
        | Functor_type(arg,_,_) when (Ident.name arg) = str -> Modtype mty (* TODO *)
        | Functor_type(_,ml1,ml2) -> match (find str ml1) with
          | Fail -> (find str ml2)
          | _ as a -> a
    in

    (* top level *)
    match path with | [] -> raise (Cannot_compile "Empty path given to lookup")
    | str :: [] -> (find str ty)
    | str :: ls -> match (find str ty) with 
      | Modtype mty -> (look_up_type_path mty ls)
      | _ -> raise (Cannot_compile "Reached a dead end in type path search")


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name: gen_full_path
  *  Description: generate a path to pth in mty using nm to check name space (* God mode *)
  * =====================================================================================
  *)
  let gen_full_path mty pth ns =

    (* establish the target *) 
    let target = (match (List.rev pth) with [] -> raise (Cannot_compile "Empty path to complete") | x::_ -> x) in

    (* search declarations *)
    let rec full_decl (topl : string list) (sigls : specification list) (ns : string list) : unit = match sigls with 
       | [] -> ()
       | x :: xs -> match x with 
         | Value_sig (id,_) when (Ident.name id) = target -> raise (Found (List.rev (pth@topl)))
         | Type_sig (id,_) when (Ident.name id) = target -> raise (Found (List.rev (pth@topl)))
         | Module_sig (id,mod_type) when (Ident.name id ) = target -> raise (Found (List.rev (pth@topl)))
         | Module_sig (id,mod_type) -> let name = (Ident.name id) in
           if (((List.length ns) > 0) && (name = (List.hd ns))) 
           then ((full_mty (name::topl) mod_type (List.tl ns)); (full_decl topl xs ns))  (* Recurse *)
           else (full_decl topl xs ns)
         | _ -> (full_decl topl xs ns)

    (* search module types *)
    and full_mty (topl : string list) (ty : mod_type) (ns : string list) : unit = match ty with
      | Signature sigls -> (full_decl topl (List.rev (sort_sigs sigls)) ns)
      | Functor_type (id,ml1,ml2) when (Ident.name id) = target -> raise (Found (List.rev (pth@topl)))
      | Functor_type (id,ml1,ml2) -> let argum = (Ident.name id) in
         (full_mty topl ml1 ns); (full_mty topl ml2 ns); ()
     
    in
    (* top level *)
    (*(Printf.eprintf "Looking for path: %s -> %s in %s\n" (String.concat "." pth) target (String.concat "." ns));*)
    (full_mty [] mty (List.rev ns))


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile_simple_type
  *  Description:  convert the simple type to the intermed representation
  * =====================================================================================
  *)
  let rec compile_simple_type progtype pth =  
    function
    | Var _ as x -> compile_simple_type progtype pth (typerepr x)
    | LambdaType(TIgnore,_) -> TyIgnore
    | LambdaType(TBool,_) -> TyBool
    | LambdaType(TInt,_) ->  TyInt
    | LambdaType(TArrow,[ty1;ty2]) -> let tu1 = compile_simple_type progtype pth ty1 in
        let tu2 = compile_simple_type progtype pth ty2 in
        TyArrow (tu1,tu2)
    | LambdaType(TPair,[ty1;ty2]) -> let tu1 = compile_simple_type progtype pth ty1 in
        let tu2 = compile_simple_type progtype pth ty2 in
        TyStar (tu1,tu2)
    | Typeconstr(path,_) -> (*(Printf.eprintf "Going for %s\n" (String.concat "." (convert_path path)));*)
      let obtainbase p = 
        (*Printf.eprintf "full path = %s\n" (String.concat "." p));*)
        (match (look_up_type_path progtype p) with
          | Fail -> raise (Cannot_compile "Type path not found")
          | SimpleType ty -> (compile_simple_type progtype p ty)
          | Modtype mty -> (compile_mty_type progtype (List.rev p) mty)
          | ManifestType opt -> (match opt with
            | None -> (TyAbstract (TyCString (String.concat "." p)))
            | Some simple -> (compile_simple_type progtype (List.rev p) simple.defbody)))
      in
      let fullpath = 
        (try (gen_full_path progtype (convert_path path) pth); 
           raise (Cannot_compile "Couldn't gen. path")
         with Found topl -> topl
         | Cannot_compile _ as a -> raise a) in
        (*(raise (Cannot_compile ("Before obtain "^(String.concat "." fullpath)))); *)
      (obtainbase fullpath)
      | _ -> raise (Cannot_compile "Cannot convert a simple type")
 

 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  parse_type
  *  Description:  compiles the lambda calculus
  * =====================================================================================
  *)
  and compile_mty_type progtype pth mty = 

    (* type declarations, without kind or param *)
    let convert_decl pth dec = match dec.manifest with
      | None -> TyAbstract (TyCString (String.concat "." (List.rev pth)))
      | Some dt -> (* TODO params *) (compile_simple_type progtype pth dt.defbody)
    in

    (* signature specifications *)
    let rec convert_spec pth = function
      | Value_sig (id,st) -> TyValue (TyCString (Ident.name id),(compile_simple_type progtype pth st.body))
      | Type_sig (id,td) -> TyDeclar (TyCString (Ident.name id),(convert_decl pth td))
      | Module_sig (id,mty) -> let name = (Ident.name id) in
        TyModule (TyCString (Ident.name id), compile_mty_type progtype (name::pth) mty)
    in

    (* top level *)
    match mty with 
    | Signature sigls -> TySignature (List.map (fun x -> convert_spec pth x) sigls)
    | Functor_type (id,mty1,mty2) -> let argum = (Ident.name id) in
      let inter1 = (compile_mty_type progtype pth mty1)
      and inter2 = (compile_mty_type progtype pth mty2) in
      TyFunctor (TyCString argum ,inter1,inter2)

 (* 
  * ===  FUNCTION  ======================================================================
  *     Name: type_weave
  *  Description:  converts the binding into lists of gettr's struct ptr's and fctr's 
  * =====================================================================================
  *)
  let type_weave progtype binding = 

    (* ================================================= *)
    (* extract the shared associations                   *)
    (* ================================================= *)
    let extract_assoc sign binding = 

      (* remove unwanted data from a signature specification *)
      let rec clear_sigs = function [] -> []
        | x :: xs -> (match x with
          | Type_sig _ -> (clear_sigs xs)
          | _ -> x :: xs)
      in

      (* remove those structure bindings that don't need to be shared *)
      let rec filter_shares sigls strls = match(sigls,strls) with ([],_) -> []
        | (s::ss,b::bs) -> (match (s,b) with
          | (Value_sig (id1,vty),BVal (name,_,_)) when (Ident.name id1) = name ->
            (*Printf.eprintf "%s == %s \n" (Ident.name id1) name) ;*)
            (b,s) :: (filter_shares ss bs) 
          | (Value_sig _, BVal _ ) -> (filter_shares (s::ss) bs)
          | (Module_sig (id1,mty), BMod (name,_)) when (Ident.name id1) = name ->
            (b,s) :: (filter_shares ss bs)
          | (Module_sig _, BMod _) -> (filter_shares (s::ss) bs)
          | (Module_sig (id1,_),BVal _) -> (filter_shares (s::ss) bs)
          | _ -> raise (Cannot_compile "Serieus failure of argument structure in Share computation"))
        | _ -> raise (Cannot_compile "Serieus failure of list structure in Share computation")
      in

      (* pipe in the easiest possible input *)
      let clear_input sign strls = match sign with 
        | Signature sigls -> (*(Printf.eprintf "%d vs %d\n" (List.length sigls) (List.length strls)); *)
            (filter_shares (clear_sigs (sort_sigs sigls)) (sort_bindings strls))
        | _ -> raise (Cannot_compile "Expected a Signature")
      in

      (* Do the conversion *)
      let rec convert_assoc path = function [] -> []
        | (bind,ty)::ls -> match (bind,ty) with
          | (BVal (name,_,_) , Value_sig (_, vty)) ->  
            (Call (vty,name,path))::(convert_assoc path ls) 
          | (BMod (name, modt), Module_sig(_,mty)) -> (match modt with 
            | SB (pth, nbinding,unique) -> let recurse = if unique 
              then (convert_assoc (name::path) (clear_input mty nbinding))
              else [] in 
                ((Share (mty,name,pth)) :: recurse) @ (convert_assoc path ls)
            | FB (pth,_,_,_) -> (Share (mty,name,pth)) :: (convert_assoc path ls)) 
          | _ -> raise (Cannot_compile "Massive idiocy everywhere")

      in

      (* top level *)
      convert_assoc [] (clear_input sign binding)
    in

    (* update the gettrs *)
    let rec update_gettrs gs = function [] -> gs
      | (Call(_,name,pth) :: cs) as ccs -> let ptr = (make_ptr (name::pth));  in
        (Printf.eprintf "%s\n" ptr);
        (match gs with 
        | [] -> []
        | g :: ggs -> (match g with
          | Gettr(str,lc,comp) when ptr = str -> (Printf.eprintf "match = %s\n" str); (g :: (update_gettrs ggs cs))
          | Gettr (str,_,comp) -> (Printf.eprintf "Miss %s\n" str); (Gettr (str,LOCAL,comp)) :: (update_gettrs ggs ccs)))
      | _ -> raise (Cannot_compile "Massive idiocy")
    in

    (* top level *)
    let (gettrs,strcts,fctrs) = (MC.High.extract [] binding) 
    and assocs = (extract_assoc progtype binding) in 
    let calls_s = (List.filter (function Call _ -> true | _ -> false) (sort_assocs assocs)) in
    let gettr_s = (sort_compred gettrs) in
    let ngettrs = (update_gettrs gettr_s calls_s) in
      (ngettrs,strcts,fctrs,assocs)


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile
  *  Description:  converts the toplevel into a giant string
  * =====================================================================================
  *)
  let compile mty program =


    (* compare statics *)
    let comp_stat a b = match a with 
      ToStatic (_,x) -> (match b with 
        ToStatic(_,y) -> (String.compare x y)
        | _ -> -1) 
      | _ -> -1 in

    (* ================================================= *)
    (* build the bootup function: where we set it all up *)
    (* ================================================= *)
    let boot_up strls assocs progtype =

      (* create the insertion binding *)
      let to_binding = function [] -> (constv TOP)
        | x :: xs as ls -> let ptr = (make_ptr ls) in
          (CVar (ptr ^ "->mod" )) 
      in
       
      (* add print the associated binding *)
      let rec print_assoc = function [] -> ([],[])
        | Call (vty,strl,pth) :: xs -> 
          let strr = CVar (make_ptr (strl::pth)) in
          let statptr = (CVar (var_prefix^"_"^strl^"_str")) in
          let static = ToStatic(statptr,strl) 
          and temp = InsertMeta ((to_binding pth),statptr,strr,1, (compile_simple_type progtype pth vty.body)) in
          let (lss,lsb) = (print_assoc xs) in
          (static::lss,temp::lsb) 
        | Share (mty,strl,pth) :: xs -> 
          let strr = CVar (make_ptr pth) 
          and bindpth = try (List.tl pth) with _ -> []
          and statptr = (CVar (var_prefix^"_"^strl^"_str")) in
          let static = ToStatic(statptr,strl) 
          and tmpmty = TyModule ((TyCString strl),(compile_mty_type progtype pth mty)) in
          let temp = InsertMeta ((to_binding bindpth),statptr,strr,0,tmpmty) in
          let (lss,lsb) = (print_assoc xs) in
          (static::lss,temp::lsb) 
      in

      (* print_strcts: convert structs into mallocs and bindings *)
      let rec print_strcts = function [] -> ([],[])
        | (Strct pth) :: xs -> (let ptr = (make_ptr pth) in
          let decl = MALLOC (STRUCTURE,(CVar ptr),(Sizeof STRUCTURE)) in
          let (dls,bls) = (print_strcts xs) in
            ((decl :: dls), bls)) 
        | _ -> raise (Cannot_compile "print_strcts only prints Strct") 
      in
        
      (* top level *)
      let def = ToDef ((CVar "int"),(CVar c_boot),[]) in
      let strdecl = (match (print_strcts strls) with 
        | (a,b) -> (a @ [Emptyline] @ b)) in
      let (statics,bindings) = (print_assoc assocs) in
      let final_sts =  (List.sort_uniq comp_stat statics) in
      let body_ls = (List.map printc (final_sts @ [Emptyline] @ strdecl @ bindings @ [Emptyline;(ToReturn (CInt 1))])) in
      ( (printc def) :: (format 1 body_ls) @ func_end) 
    in

    (* print fnctrs TODO *)
    let rec print_fctrs = function [] -> []
      | (Fctr (name,loc)) as f :: xs -> let definition = printf (MC.Low.funcdef false f) in
        let body = (format 1 ["return;"]) in 
        (String.concat "\n" ( (definition::body) @ func_end)) :: (print_fctrs xs) 
      | _ -> raise (Cannot_compile "print_fctrs - only compiles Gettr")
    in

    (* header and footer for the compiled result *)
    let header = ["// Compiled by lanren"; "#include \"miniml.h\"";  ""] in
    let separate name = function [] -> []
      | x::xs as ls -> ["//---------------"^ name ^ "----------------------"; ""] @ ls @ [""] in
    let footer = ["// Include the entrypoints & binding code"; "#include \"binding.c\""; "#include \"data.c\"" ; 
            "#include \"entry.c\""; ""] in

    (* Top Level *)
    let (lambda_list,omega) = (MC.High.compile program) in
    let (gettr_lst,strct_list,fctr_list,assocs) = type_weave mty omega in 
    let dec_ls = (separate "declarations" (List.map printf 
                   (List.map (fun x -> (MC.Low.funcdef true x)) gettr_lst)))
    and pl_ls =  (separate "Closures" (MC.Low.lambda (List.rev lambda_list)))
    and pv_ls =  (separate "Values" (MC.Low.getter (List.rev gettr_lst)))
    and fc_ls = (separate "Functors" (print_fctrs (List.rev fctr_list)))
    and pb_ls = (separate "Boot" (boot_up strct_list assocs mty)) in
    (((String.concat "\n"  (header @ dec_ls @ pl_ls @ pv_ls @ fc_ls @ pb_ls @ footer)) ^ "\n"),"hello there")
  
end

