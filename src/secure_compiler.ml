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
  exception Cannot_Sec_Compile of string


 (*-----------------------------------------------------------------------------
  *  Types
  *-----------------------------------------------------------------------------*)

  type compilertype = MiniMLMod.mod_type -> MiniMLMod.mod_term -> string -> (string * string)

  type typetrawl = Fail | SimpleType of simple_type | Modtype of mod_type 
                 | ManifestType of def_type option

  type fcheck = Easy of int | Painfull of string 
  type mask = (fcheck * string)
  type argument = ( tempc * string * cpath)
  type func = mask option * argument option

  type assoc = Call of val_type * string * string list * cpath * func 
             | Share of MiniMLMod.mod_type * string * string list * string list * func
             | Provide of MiniMLMod.mod_type * string * string * string list * string list * func
             | Constrain of MiniMLMod.mod_type * string * cpath * (string * accs * string) list * func

  type methods = EntryPoint of string * type_u * computation * mask option
 

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
      | (BArg _,_) -> raise FailSort
      | (_,BArg _) -> raise FailSort
      | (BVal (name1,_,_) , BVal(name2,_,_)) -> (String.compare name1 name2)
      | (BMod (name1,_,_) , BMod(name2,_,_)) -> (String.compare name1 name2)
      | (BVal _, _) -> -1
      | (BMod _, _) -> 1
    in
    (List.sort cmp_binding bls)

  (* sort compiler redices *)
  let sort_compred cls =
    let cmp_red a b = match (a,b) with
      | (Gettr(str,_,_,_,_),Gettr(str2,_,_,_,_)) -> (String.compare str str2)
      | (Strct (_,n,pth,_,_,_,_) , Strct (_,n2,pth2,_,_,_,_)) -> (String.compare (make_ptr (n::pth)) (make_ptr (n2::pth2)))
      | (Fctr (str,_,_),Fctr (str2,_,_)) -> (String.compare str str2)
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
    let cmp_pth l r = (String.compare (make_ptr l) (make_ptr r)) in
    let cmp_ass a b = match (a,b) with 
      | (Call (_,n,pth,_,_),Call (_,n2,pth2,_,_)) -> (cmp_pth (n::pth) (n2::pth2))
      | (Share (_,n,_,pth,_),Share (_,n2,_,pth2,_)) -> (cmp_pth (n::pth) (n2::pth2))
      | (Provide (_,n,_,_,pth,_),Provide (_,n2,_,_,pth2,_)) -> (cmp_pth (n::pth) (n2::pth2))
      | (Constrain (_,n,pth,_,_), Constrain (_,n2,pth2,_,_)) -> (cmp_pth (n::pth) (n2::pth2))
      | (Call _, _)  -> 1
      | (Provide _, _) -> -1
      | (Share _,Call _) -> -1
      | (Share _,Constrain _) -> 1
      | (Share _,Provide _) -> 1
      | (Constrain _,Call _) -> -1
      | (Constrain _,Share _) -> -1
      | (Constrain _,Provide _) -> 1
    in
    (List.sort cmp_ass als)

  (* sort per path *)
  let sort_assocspth als =
    let ccmp a b =  
      if ((List.length a) == (List.length b))
      then (String.compare (make_ptr a) (make_ptr b))
      else (Pervasives.compare (List.length a) (List.length b))
    in
    let cmp_ass a b = match (a,b) with
      | (Call (_,_,p,_,_), Call (_,_,p2,_,_)) -> ccmp p p2
      | (Constrain (_,_,p,_,_), Constrain (_,_,p2,_,_)) -> ccmp p p2
      | (Share (_,_,_,p,_), Share (_,_,_,p2,_)) -> ccmp p p2
      | (Provide (_,_,_,_,p,_), Provide (_,_,_,_,p2,_)) -> ccmp p p2
      | (Call (_,_,p,_,_), Share (_,_,_,p2,_)) -> ccmp p p2
      | (Call (_,_,p,_,_), Provide (_,_,_,_,p2,_)) -> ccmp p p2
      | (Call (_,_,p,_,_), Constrain (_,_,p2,_,_)) -> ccmp p p2
      | (Share (_,_,_,p,_), Call(_,_,p2,_,_)) -> ccmp p p2
      | (Share (_,_,_,p,_), Constrain(_,_,p2,_,_)) -> ccmp p p2
      | (Share (_,_,_,p,_), Provide (_,_,_,_,p2,_)) -> ccmp p p2
      | (Provide (_,_,_,_,p,_), Share (_,_,_,p2,_)) -> ccmp p p2
      | (Provide (_,_,_,_,p,_), Call (_,_,p2,_,_)) -> ccmp p p2
      | (Provide (_,_,_,_,p,_), Constrain (_,_,p2,_,_)) -> ccmp p p2
      | (Constrain (_,_,p,_,_), Share (_,_,_,p2,_)) -> ccmp p p2
      | (Constrain (_,_,p,_,_), Provide (_,_,_,_,p2,_)) -> ccmp p p2
      | (Constrain (_,_,p,_,_), Call (_,_,p2,_,_)) -> ccmp p p2
    in
    (List.sort cmp_ass als)
     

  (* split associations into a sequence of path related assocs *)
  let split_assocpth als =
    let rec filtr (tar : string list) (curr : assoc list) : assoc list -> assoc list list = function [] -> curr :: []
      | l :: ls -> match l with
        | Call (_,_,p,_,_) as c when (make_ptr p) = (make_ptr tar) ->
            (filtr tar (c::curr) ls)
        | Call (_,_,p,_,_) as c -> curr :: (filtr p [c] ls)
        | Share (_,str,_,path,_) as s when (make_ptr path) = (make_ptr tar) ->
            (filtr tar (s::curr) ls)
        | Share (_,str,p,path,_) as s -> curr :: (filtr path [s] ls)
        | Provide (_,str,_,_,path,_) as f when (make_ptr path) = (make_ptr tar) ->
            (filtr tar (f::curr) ls)
        | Provide (_,str,_,_,path,_) as f -> curr :: (filtr path [f] ls)
        | Constrain (_,_,p,_,_) as c when (make_ptr p) = (make_ptr tar) ->
            (filtr tar (c::curr) ls)
        | Constrain (_,_,p,_,_) as c -> curr :: (filtr p [c] ls)
    in
    (List.rev (filtr [] [] (sort_assocspth als)))
          
  (* compare statics *)
  let cmp_stat a b = match a with 
    ToStatic (_,Assign(_,CString id)) -> (match b with 
      ToStatic(_,Assign(_,CString id2)) -> (String.compare id id2)
      | _ -> -1) 
    | _ -> -1 


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
           else if (List.length ns == 0)
           then ((full_mty (name::topl) mod_type []); (full_decl topl xs ns))  (* Recurse *)
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
         with Found topl -> (*(Printf.eprintf "found %s \n" (String.concat "." topl)*) topl
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

    (* crazy list *)
    let crazy_lst = ref [] in

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
          | (Module_sig (id1,mty), BMod (name,_,_)) when (Ident.name id1) = name ->
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
          | (BVal (name,pth,_) , Value_sig (_, vty)) ->
            (Call (vty,name,path,pth,(None,None)))::(convert_assoc path ls) 
          | (BMod (name,_, modt), Module_sig(_,mty)) -> (match modt with 
            | AR _ -> raise (Cannot_Sec_Compile "Cannot convert AR modbinding")
            | SB (pth, nbinding,_) -> 
              let recurse = convert_assoc (name::path) (clear_input mty nbinding) in
                ((Share (mty,name,pth,path,(None,None))) :: recurse) @ (convert_assoc path ls)
            | FB (pth,var,_,_,mb,id,_) as y -> let head = Provide (mty,name,var,pth,path,(None,None)) in 
              head :: (convert_fassoc name path y mty) @ (convert_assoc path ls)) 
          | _ -> raise (Cannot_Sec_Compile "Massive idiocy everywhere")


      (* ================================================= *)
      (* Functor association computation                   *)
      (* ================================================= *)
      and convert_fassoc name path func mty = 

        (* no sorting here *)
        let find name ls = 
          let predicate  = function
            | BVal (nn,_,_) when name = nn -> true
            | BMod (nn,_,_) when name = nn -> true
            | BArg _ -> raise (Cannot_Sec_Compile "Functor Argument shouldn't be here")
            | _ -> false
          in
          try (List.find predicate ls) 
          with Not_found -> raise (Cannot_Sec_Compile "binding not found")
        in

        (* Compile an AR once for use in the entry point *)
        let compile_ar ar = 

          let make_setup comp = let arg = (var_prefix ^"arg") in
            let assign = Assign(CVar ((printd MODULE)^" "^arg),comp) in 
              (assign,arg,[])
          in

          let rec parse : modbindtype -> tempc = function
            | AR (n,None,None,_) -> (ToCall ((CVar "path_module"),[(constv MOD);(CString n)]))
            | AR (n,Some ls,None,_) ->  let pp =  (make_path ls) in
                (ToCall ((CVar "path_module"),[(constv MOD) ; (CString pp) ]))
            | AR (n,ls,Some mb,i) ->  let ar = (parse (AR(n,ls,None,i))) 
              and arg = (parse mb) in
                (ToCall(ToFunctor(ar),[(constv MOD);arg]))
            | FB (pth,var,_,_,mb,id,(dyn,_)) -> if not dyn then (CVar (make_str pth)) 
                else ToCall(CVar (make_ptr pth),[(constv MOD)])
            | SB (pth,nbinding,(dyn,_)) -> if not dyn then (CVar (make_str pth))
                else ToCall(CVar (make_ptr pth),[(constv MOD)]) 
          in

          make_setup (parse ar)
        in

        (* extend the argument produced by compile_ar *)
        let extend_argument extra = function
          | (a,b,ls) -> (a,b,extra :: ls)
        in

        (* parse the signature spec *)
        let rec parse_sigls needs path nbinding = function [] -> []
          | x :: xs -> match x with
            | Value_sig (id, vty) -> let nm = Ident.name id in  
                let BVal (_,pth,_) = (find nm nbinding) in
                Call(vty,nm,path,pth,needs) :: (parse_sigls needs path nbinding xs)
            | Module_sig (id,mty) -> let nm = Ident.name id in
                let BMod (_,_,cont) = (find nm nbinding) in
                (parse_mty needs false nm path cont mty) @ (parse_sigls needs path nbinding xs)
            | Type_sig _ -> (parse_sigls needs path nbinding xs)

        (* quickly extract top level entry points *)
        and quick path = function [] -> []
          | x :: xs -> match x with
            | Value_sig(id,_) -> let nm = (Ident.name id) in 
                (nm,BDVAL,(make_entrypoint (nm::path))) :: (quick path xs)
            | Module_sig(id,_) -> let nm = (Ident.name id) in
                (nm,BDMOD,(make_entrypoint (nm::path))) :: (quick path xs)
            | Type_sig _ -> (quick path xs)

        (* build the extra shares for the arguments *)
        and build_arg path need arg ar = function [] -> []
          | x :: xs -> match x with
            | Value_sig(id,vty) -> let nm = (Ident.name id) in 
              let newarg = let (a,b,c) = (extend_argument nm arg) in
                let bb = (match ar with 
                  | AR(_,_,Some _,_) -> b
                  | AR(_,_,None,_) -> "m->m")
                in
               Some (a,bb,c)
               in 
               Call(vty,nm,path,path,(need,newarg)) :: (build_arg path need arg ar xs)
            | Module_sig (id,mty) -> let nm = (Ident.name id) in
              let newarg  = (extend_argument nm arg) in 
              (parse_mty (need,Some newarg) false nm path ar mty) @ (build_arg path need arg ar xs)
              
        (* parse the module type *)
        and parse_mty needs self mname npath cont mty = 

          let (need,arg) = needs in  

          (* this is driving me crazy *)
          let no_self head next = if (not self) 
            then head :: next
            else next
          in
      
          let updated_arg a = match a with
            | None ->  (compile_ar cont)
            | Some arg -> arg 
          in

          match (cont,mty) with

          | (FB(pth,var,_,_,mb,idn,_), Functor_type (id,lmty,rmty))-> 
              let nneeds = (Some (Easy idn,var ),arg) in
              let head = Provide (mty,mname,(Ident.name id),pth,npath,nneeds)  
              and next = (parse_mty nneeds self "Functor" (mname::npath) mb rmty) in
              (no_self head next)

          | (SB (pth,nbinding,_),(Signature sigls)) -> 
              let head = Share(mty,mname,pth,npath,needs) 
              and next = (parse_sigls needs (mname::npath) nbinding sigls) in
              (no_self head next)

          | (AR _,(Signature sigls)) -> 
              let arg = updated_arg arg in
              let triple = (quick (mname::npath) sigls) in 
              let head = Constrain (mty,mname,npath,triple,(need,Some arg))
              and next = (build_arg (mname::npath) need arg cont sigls) in
              (if self then crazy_lst := (name,path,triple) :: !crazy_lst);
              (no_self head next)

          | (AR (_,_,_,c), Functor_type (id,lmty,rmty) ) -> 
              let arg = updated_arg arg in
              let (_,ptr,_) = arg in
              let nneeds = (Some ((Easy c),(Ident.name id)),Some arg) in
              let head = Provide (mty,mname,(Ident.name id),npath,npath,nneeds)
              and next = (parse_mty nneeds self "Functor" (mname::npath) cont rmty) in
              (no_self head next)

         (* | (AR(n,ls,Some arg),Functor_type (id,lmty,rmty)) -> DEPRECATED *) 
              
        in

        (* top level*)
        (parse_mty (None,None) true name path func mty)
      in

      (* top level *)
      (Share (progtype,"this",[],[],(None,None))) :: (convert_assoc [] (clear_input sign binding)) 
    in


    (* ================================================= *)
    (* Compile the entrypoints                           *)
    (* ================================================= *)
    let compile_entrypoints assocs gettrls =

      (* new_variables *)
      let new_var = let count = ref (-1) in 
        fun () -> incr count; (var_prefix^"_s_" ^ (string_of_int !count)) 
      in

      (* new mask *)
      let new_mask = let count = ref (-1) in
        (fun () -> incr count; !count) 
      in

      (* new mask *)
      let new_maskf = let count = ref (-1) in
        (fun () -> incr count; !count) 
      in

      (* helpers *)
      let convm p m = ToCall ((CVar "convertM"), [p ; m]) in

      let convf f v p = 
        let predicate = (fun x -> if x = "Functor" then false else true) in
        let path = make_path (List.filter predicate p) in
        ToCall(f,[CVar v; CString path])
      in

      let empty c = ([],[],c) in 

      (* convert assocs into Gettrs *)
      let rec entrypts : assoc list -> methods list = function [] -> []
        | x :: xs -> match x with

          | (Call(ty,name,pth,rpth,(needs,arg))) -> 
            let eptr = (make_entrypoint (name::pth))  in
            let ptr = make_ptr (name::rpth) in
            let tycomp = CVar (printty (compile_simple_type progtype pth ty.body)) in
            let comp = (match arg with
              | None -> empty (ToCall ((constc CONV),[ToCall((CVar ptr),[(constv MOD)]) ; tycomp ]))
              | Some (c,var,p) -> let fcall = convf (CVar "get_value_path") var p  in
                ([],[c],(ToCall ((constc CONV),[fcall; tycomp]))) ) in
            EntryPoint(eptr,(constd DATA),comp,needs) :: (entrypts xs)

          | Share(mty,name,opth,pth,(needs,arg))  -> 
            let eptr = (make_entrypoint (name::pth)) in
            let modcomp = CVar (printty (compile_mty_type progtype opth mty)) in
            if name = "this" then (* This is awful *)
              let special = CVar (make_str []) in
              let compu =  empty (convm special modcomp) in
              EntryPoint(name,(constd MODDATA),compu,needs) :: (entrypts xs)
            else
              let compu = (match arg with 
                | None -> (match needs with
                  | None -> empty (convm (CVar (make_str (name::pth))) modcomp)
                  | Some _ -> empty (convm (ToCall (CVar (make_ptr (name::pth)),[(constv MOD)])) modcomp))
                | Some (c,var,p) -> let fcall = convf (CVar "get_module_path") var p in  
                  ([],[c],(convm fcall modcomp)))
              in
              EntryPoint(eptr,(constd MODDATA),compu,needs) :: (entrypts xs)

          | Provide(mty,name,id,opth,pth,(needs,arg)) -> 
            let eptr = (make_entrypoint (name::pth)) in
            let modcomp = CVar (printty (compile_mty_type progtype opth mty)) in
            let comp = (match arg with 
             | None -> empty (convm (CVar (make_str (name::pth))) modcomp)   
             | Some (c,var,p) -> let fcall = convf (CVar "get_module_path") var p in  
                ([],[c],(convm fcall modcomp)))
            in
            EntryPoint(eptr,(constd MODDATA),comp,needs) :: (entrypts xs)

          | Constrain (mty,name,path,ls,(needs,arg)) ->
            let names = List.map (function (x,_,_) -> x) ls in
            let assocs = List.map (function (_,x,_) -> x) ls in
            let eptrs = List.map (function (_,_,x) -> x) ls in
            let eptr = (make_entrypoint (name::path)) in
            let modcomp = CVar (printty (compile_mty_type progtype path mty)) in
            let (extra,target) = (match arg with 
              | None -> ([],(ToCall (CVar (make_ptr (name::path)),[(constv MOD)]))) 
              | Some (c,var,p) -> let fcall = convf (CVar "get_module_path") var p in
                ([c],fcall))
            and eonvs = (convert_entry assocs) in
            let nchars = String.concat "," (List.map (fun x -> (printc (CString x))) names)
            and echars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") eonvs eptrs) in
            let nv = ("names"^(make_var (name::path))) 
            and ev = ("ptrs"^(make_var (name::path))) in
            let nls =  (printd CHAR)^" "^nv^"[] = {"^nchars^"}" 
            and els = (printd ENTRY)^" "^ev^"[] = {"^echars^"}" in
            let stamp = (match needs with
                | Some (Easy i,_) ->  CInt i          
                | Some (Painfull str,_) -> CVar (str^".stamp")
                | _ -> raise (Cannot_Sec_Compile "Idiocy in front of the Forbidden Palace")) 
            in
            let ptr = ToCall (CVar "updateEntry",
               [ target ; (constv MOD); stamp; CInt (List.length names); CVar nv ; CVar ev ]) in
            let compu = ([],extra @ [CVar nls; CVar els],ToCall ((CVar "convertM"), [ptr ; modcomp])) in
            EntryPoint(eptr,(constd MODDATA),compu,needs) :: (entrypts xs)
      in

      (* top level *)
      (entrypts assocs)
    in

    (* update_compred *)
    let update_compred ls = 
      let update = function
        | Gettr(str,dtstr,_,mask,comp) -> Gettr(str,dtstr,LOCAL,mask,comp)
        | Fctr(p,_,c) -> Fctr(p,LOCAL,c) 
        | _ -> raise (Cannot_Sec_Compile "updating the wrong redices")
      in
      (List.map update ls)
    in

    (* top level *)
    let (gettrs,strcts,fctrs) = (MC.High.extract_red binding) 
    and assocs  = List.rev (sort_assocspth (extract_assoc progtype binding)) in 
    let gettr_s = (sort_compred gettrs) in
    let ngettrs = update_compred gettr_s in
    let nfctrs = update_compred fctrs in
    let gentry = compile_entrypoints assocs ngettrs in
      (!crazy_lst,gentry,ngettrs,strcts,nfctrs,assocs)


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  compile
  *  Description:  converts the toplevel into a tuple of 2 strings for object & header
  * =====================================================================================
  *)
  let compile mty program headerf =

    (* print entrypoints *)
    let rec entrypoint = function EntryPoint (name,typ,comp,mask) ->
      let args = match mask with 
        | Some _ -> [ (MODDATA,STR) ]  
        | None -> [] 
      in
      let definition = (ENTRYPOINT,typ,name,args,false) in
      let md = (printd BINDING)^" "^(printconst MOD)^" = NULL" in
      let (ign,poss,c) = comp in
      let check = match mask with 
        | None -> [] 
        | Some (fcheck,var) -> 
            let sv = (CVar "union safe_cast s")
            and sc = (Assign ((CVar "s.value"),ToIdent (constv STR)))
            and gc = (ToCall((CVar "getBinding"),[(CVar "exchange");ToByte (CVar "s"); (constc CMP_INT)])) in 
            let gt = (Assign ((CVar "struct module_type * m"),gc)) 
            and stamp = match fcheck with
              | Easy i ->  CInt i           
              | Painfull str -> CVar (str^".stamp")
            in
            let cm = (ToCall((CVar "checkModule"),[(CVar "m->m") ; stamp])) 
            and up = (Assign((constv MOD),(CVar "m->m.strls"))) in
            [sv;sc;gt;up] @ poss @ [cm]
      in
      let newc = (ign,check,c) in
      let body = (format 1 ( md :: (MC.Low.computation newc))) in
        (String.concat "\n" ( ((printf definition)::body) @ func_end ) ) 
    in

    (* map entry point definitions *)
    let mapentrydef ls = (List.map printf 
      (List.map (function EntryPoint(name,typ,comp,mask) ->
        let args = match mask with | Some _ -> [ (MODDATA,STR) ] | _ -> [] in
        (ENTRYPOINT,typ,name,args,true)) ls))
    in

    (* is a real entrypoint *)
    let update_entry cls enls str =

      (* is an entrypoint *)
      let isentry ptr = 
        let predicate = function
          | EntryPoint (str1,_,_,_) when str1 = ptr -> true
          | _ -> false
        in 
        (List.exists predicate enls) 
      in

      (* is a special fctr *)
      let isspecial ptr =
        let predicate = function
          | (n,pth,_) -> (make_str (n::pth)) = ptr
        in
        (List.find predicate cls)
      in

      (* convert non entry point to NULL *)
      let conv_e = (function 
        | x when (isentry x) -> x  
        | _ -> "NULL") 
      in
      match str with
      | Strct (Fun,n,pth,info,_,_,_) -> (try let (_,_,ls) = isspecial (make_str (n::pth)) in
          let names = List.map (function (x,_,_) -> x) ls in
          let assocs = List.map (function (_,x,_) -> x) ls in
          let eptrs = List.map (function (_,_,x) -> x) ls in
          Strct (Fun,n,pth,info,assocs,names,eptrs) 
        with Not_found -> str)
      | Strct (t,a,b,c,d,e,ls) -> let nls = (List.map conv_e ls) in 
        Strct (t,a,b,c,d,e,nls)
      | _ -> raise (Cannot_Sec_Compile "only strct can deal with entry conversion")
      (*Fctr (a,b,c,ls) -> let rec conv = function [] -> []
          | ys::(x::xs)::bls -> ys::(x :: (List.map conv_e xs)) :: (conv bls) 
        in
        let nls = (conv ls) in
        Fctr (a,b,c,nls) *)
    in

    (* convert list of compiler redices into strings of function definitions *)
    let mapfd ls = (List.map printf 
      (List.map (fun x -> (MC.Low.funcdef true x)) ls))
    in

    (* Top Level *)
    let (lambda_list,omega) = (MC.High.compile program) in
    let (crazy,gentry,gettr_lst,strct_list,fctr_list,assocs) = type_weave mty omega in 


    (* filter out the unnecessary entry points and add new ones *)
    let n_strlist = (List.map (fun x -> (update_entry crazy gentry x)) strct_list)
    (*and n_fctrlist = (List.map (fun x -> (convert_entry gentry x)) fctr_list)*)
    in

    (* build the header *)
    (*let str_ls = (separate "Structs" (format 0 (List.map printc (print_strc (split_assocpth assocs)))))*)
    let en_dls = (separate "Entry Points" (mapentrydef gentry))
    and hedh = header  (List.map printc [(consth ENTRY)]) in
    let headerfile = (String.concat "\n"(hedh @ en_dls)) ^ "\n"  
    in

    (* build the object file *)
    let dec_ls = (separate "Declarations" (mapfd (gettr_lst@fctr_list)))
    and pb_ls = (separate "Static Structures" (MC.Low.structure n_strlist))
    and pl_ls = (separate "Closures" (MC.Low.lambda (List.rev lambda_list)))
    and pv_ls = (separate "Values" (MC.Low.getter (List.rev gettr_lst)))
    and en_ls = (separate "Entry Points" (List.map entrypoint gentry))
    and fc_ls = (separate "Functors" (MC.Low.lambdaf (List.rev fctr_list)))
   (* and pb_ls = (separate "Boot" (boot_up strct_list assocs mty)) *)
    and objh =  header (List.map printc [(Include headerf) ; (consth MINI)])
    in

    (* the two files *)
    let objectfile = ((String.concat "\n"  (objh @ dec_ls @ pb_ls @ pl_ls @ pv_ls @ fc_ls @ en_ls @ footer)) ^ "\n")
    in (objectfile,headerfile)
  
end

