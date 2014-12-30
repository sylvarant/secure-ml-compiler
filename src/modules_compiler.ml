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

  type mask = int

  and modbindtype = FB of cpath * string * MiniMLMod.mod_term * modbindtype * bool
                  | SB of cpath * strctbinding list * bool
                  | AR of string * string list option * modbindtype option

  and strctbinding = BVal of string * cpath * computation 
                   | BMod of string * cpath * modbindtype 
                   | BArg of string  (* functor argument *)

  and computation = Intermediary.tempc list * Intermediary.tempc list * Intermediary.tempc 

  (* the results of look ups into the omega environement *)
  and trawl = Static of string 
            | Environment of modbindtype
            | Dynamic of string * string list option

  and omega = strctbinding list

  type compred = Gettr of string * Intermediary.type_u * Intermediary.locality * mask option * computation 
               | Strct of string * cpath * string list * Intermediary.accs list * string list 
               | Fctr of  string * Intermediary.locality * computation
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

  let make_str lst = "str" ^ (make_ptr lst)

  let make_var lst = "var"^ (make_ptr lst)

  (* the difference between a ptr and a path is the path is the . *)
  let make_path = function [] -> raise (Omega_miss "Not a path")
    | lst  -> (String.concat "." (List.rev lst))

  (* get_static *)
  let rec get_static = function
    | Static str -> str
    | _ -> raise (Omega_miss "Environment instead of Static")

  (* convert between Modules and Fields *)
  let convert_mod ls = 
    (List.map (function BVAL -> ".gettr = " | BMOD -> ".module = &") ls)

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
      | BMod (nn,_,_) when (nn = x) -> true
      | BArg nn when (nn = x) -> true
      | _ ->  false) in
      try (List.find (find_binding) nenv)
      with Not_found -> raise (Omega_miss "Identifier not found") 
    in

    (* When it comes to values all that matters is that they exist *)
    let extract = function
        | BVal (nn,pth,_) -> Static (make_ptr (nn::pth))
        | BMod (_,_,e) -> (Environment (set_origin e)) 
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
              | Dynamic (n,ls) -> AR (n,ls,None)
              | _ -> raise (Cannot_compile_module "Did not retrieve environment from path lookup"))
          | Structure strls -> let parsed = (parse_struct env pth strls) in SB (pth,parsed,true)
          | Functor (id,ty,m) -> let bind = BArg (Ident.name id) in
            let mb = (parse_module (bind::env) ("Functor"::pth) m) in (* compile functor cont with an arg *)
              FB (pth,(Ident.name id),m,mb,true)
          | Apply (m1,m2) -> 
            (match (parse_module env pth m1) with
              | FB (_,id,m,_,_) -> let nenv = (parse_module env pth m2) in
                (parse_module ((BMod (id,path,nenv))::env) pth m)
              | AR (n,ls,None) -> let nm = (parse_module env pth m2) in 
                (AR (n,ls, (Some nm)))
              | _ -> raise (Cannot_compile_module "Needed Functor or AR hole"))
          | Constraint (m,ty) -> (parse_module env pth m) 
        in

        (* recurse over the list of definitions *)
        match strctls with [] -> []
          | x::xs ->  match x with 
              Type_str (id2,_,_) -> (parse_struct env path xs)
            | Module_str (id2,mterm) ->
              let name = (Ident.name id2) in
              let nenv = (parse_module env (name::path) mterm) in
              let value = BMod (name,path,nenv) in
              value :: (parse_struct (value :: env) path xs)
            | Value_str (id2,term) ->
              let name = (Ident.name id2) in
              let (flist,comp) = (ExprComp.compile env (name::path) term) in 
              functlist := flist @ !functlist;
              let data = BVal (name, path,comp) in 
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
    let extract_red ls = 

      (* new functor name *)
      let new_fctr = let count = ref (-1) in
        fun () -> incr count; (("Fctr_"^(string_of_int !count)),!count) in

      (* new module *)
      let new_module = let count = ref (-1) in
        fun () -> incr count; !count in

      (* fnctr list -- similar to lab *)
      let fctr_list = ref [] in

      (* quickly extract first level associations and ptrs *)
      let quick ls =
        let convert = function 
          | BArg  _ -> raise (Cannot_compile_module "Cannot convert BArg")
          | BVal (name,path,_) -> ((name,Interm.BVAL),(make_ptr (name::path)))
          | BMod (name,path,_) -> ((name,Interm.BMOD),(make_str (name::path)))
        in
        let (l,ptr) = (List.split (List.map convert ls)) in
        let (n,a) = (List.split l) in
        (n,a,ptr)
      in

      (* extract general redices *)
      let rec extract = function [] -> ([],[]) 
        | str::ls -> (match str with
          | BArg _ -> raise (Cannot_compile_module "Cannot extract BArg")
          | BVal (name,path, comp) -> let ptr = make_ptr (name::path) 
            and (a,b) = (extract ls) in
            ((Gettr (ptr,(Interm.constd Interm.VALUE),Interm.ENTRYPOINT,None,comp)) :: a, b)
          | BMod (name,path, modt) -> (match modt with
            | AR _ -> raise (Cannot_compile_module "Cannot extract AR")
            | SB (_,nbinding,true) ->  let (aa,bb) = (extract nbinding)  
              and (names,assocs,ptrs) = (quick nbinding)
              and (a,b) = (extract ls) in
              (aa @ a, Strct (name,path,names,assocs,ptrs) :: bb @ b)
            | SB (pth,_,false) ->  let (a,b) = (extract ls) in
              (a,Strct (name,path,pth,[],[]) :: b)
            | FB (pth,var,mm,mb,true) -> let npth = (make_ptr pth) 
              and (postfix,id) = new_fctr() in
              let comp = (compile_fctr ("Functor"::pth) (Some id) mb) 
              (*let cmp =([],[],Interm.ToCall ((Interm.CVar "emptyModule"),[])) *)
              and fcpth = npth ^ postfix  
              and (a,b) = (extract ls) in
              fctr_list := (Fctr (fcpth,Interm.LOCAL,comp)) :: !fctr_list;
              (a,Strct(name,path,[],[],fcpth::[]) :: b)
            | FB (pth,_,_,_,false) -> let (a,b) = extract ls in 
              (a,Strct(name,path,pth,[],[]))))


      (* ================================================= *)
      (* Compile the functor                               *)
      (* ================================================= *)
      and compile_fctr path fctr mb = 

        (* special gettrs *)
        let gettrls = ref [] in
        let setup_list = ref [] in

        (* decouple a triple list *)
        let decouple ls = let (l,ptr) = (List.split (List.map convert ls)) in
            let (n,a) = (List.split l) 
        in

        let rec parse : modbindtype -> tempc = function
          | AR(n,ls,None) -> let pp = CString (make_path ls)
            and i = CInt (List.length ls) 
            and get = (GetStr ((constv STR),(CString n))) in
            (ToCall ((constc PATH),[get ; pp ; i ]))
          | AR(n,ls,Some mb) -> let ar = (parse AR(n,ls,None)) 
            and arg = (parse mb) in
            (ToCall(ToFunctor(ar),[arg])) (* functor call *)
          | FB (pth,var,_,mb,true) -> let npth = (make_ptr pth) 
            and (postfix,id) = new_fctr() in
            let comp = (compile_fctr ("Functor"::pth) (Some id) mb) in 
            and fcpth = npth ^ postfix in 
            fctr_list := (Fctr (fcpth,Interm.LOCAL,comp)) :: !fctr_list;
            let c = ToCall ((CVar "makeContentF"),[(CVar fcpth)]) in
              ToCall(CVar "makeModule",[CInt (-1); (CVar "FUNCTOR"); (CInt fctr); (CVar "NULL"); c])
          | FB (pth,var,_,mb,false) -> (CVar (make_str pth)) 
          | SB (pth,nbinding,true) -> let (names,assocs,ptrs) = decouple (parse_str nbinding path) in
            (* the names list *)
            let nchars = String.concat "," (List.map (fun x -> (printc (CString x))) names) in
            let nptr = ("char"^(make_var (n::pth))) in
            let nls = (printd CHAR)^"* "^nptr^"[] = {"^nchars^"}" in
            (* the accosiations *)
            let achars = String.concat "," (List.map printa assocs) in
            let aptr = ("acc"^(make_var (n::pth))) in
            let als = (printd ACC)^" "^aptr^"[] = {"^achars^"}" in
            (* the Fields *)
            let convs = convert_mod assocs in
            let fchars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") convs ptrs) in
            let fptr = ("field"^(make_var (n::pth))) in
            let fls = (printd FIELD)^" "^fptr^"[] = {"^fchars^"}" in
            (* update setup *)
            setup_list := (CVar nls) :: (CVar als) :: (CVar fls) :: !setup_list;
            (* make the content and module *)
            let c = ToCall (CVar "makeContentS",[CInt (List.length names); CVar nptr; CVar aptr; CVar fptr]) in
              ToCall(CVar "makeModule",[CInt (-1); (CVar "STRUCTURE"); (CInt 0); (CVar "NULL"); c ])
          | SB (pth,nbinding,false) -> (CVar (make_str pth))
               
        and rec parse_str path = function [] -> []
          | x :: xs -> match x with
            | BArg _ -> raise (Cannot_compile_module "This shouldn't be here")
            | BVal (name,path, comp) -> let value = (Interm.constd Interm.VALUE) in
              let nn = (make_ptr (name::path))
              let get = Gettr (nn,value,fctr,comp,Some fctr) in
              gettrls := get::gettrls;
              ((name,Interm.BVAL),nn) :: (parse_str xs)
          | BMod (name,path,modt) ->  let modu = parse modt in
              let varn = (make_var "module" (name::path)) in
              let setup  = MALLOC ((constd MODULE), CVar varn,Sizeof(constd MODULE)) in
              let assign = ASSIGN (Ptr(CVar varn),modu) in
              ((name,Interm.BMOD),varn) :: (parse_str xs)

        (* toplevel *)
        ([],!setup_list,(parse mb))
      in

      (* toplevel *)
      let (gls,stls) = (extract ls) in (gls,stls,!fctr_list)

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
      | Gettr (ptr,dtstr,loc,_,_) -> (LOCAL,dtstr,ptr,[(BINDING,MOD)],b)
      | Fctr (ptr,loc,_) -> (loc,(constd MODULE),ptr,[(MODULE,STR)],b)
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
      | Gettr  (ptr,dtstr,loc,maskc,comp) :: xs -> 
        let args = [ (BINDING,MOD) ] in
        let definition = (loc,dtstr,ptr,args,false) in
        let env = (printd BINDING)^" "^(printconst ENV)^" = NULL" 
        (*and md = (printd BINDING)^" "^(printconst MOD)^" = "^(printconst MOD)*) in
        let setup = [env] in
        let body = (format 1 (setup @ (computation comp))) in
        (String.concat "\n" ( ((printf definition)::body) @ func_end ) ) :: (getter xs) 
      | _ -> raise (Cannot_convert_intermediary "print_getters - only compiles Gettr")

   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  structure
    *  Description: print a structure
    * =====================================================================================
    *)
    let rec structure : compred list -> string list  = function [] -> []
      | Strct (n,pth, p::ps as opth,[],[]) -> let name = (make_str (n::pth)) 
        and original = (make_str opth) in
        let decl = (printd MODULE)^" "^name^" = "^original in
        let body = (format 0 [decl]) in
          (String.concat "\n" (body::[""])) :: (structure xs)
      | Strct (n,pth,[],[],fctr::[]) -> let name = (make_str (n::pth))    
        let fc = "c.f={.Functor ="^fctr^"}" in
        let decl = ((printd MODULE)^" "^name^" = {"^
          (String.concat "," [".mask = "^(string_of_int 2);".type = FUNCTOR";".stamp = 0"; fc])^"}") in
        let body = (format 0 [decl]) in 
          (String.concat "\n" (body@[""])) :: (structure xs)
      | Strct (n,pth,names,assocs,ptrs) :: xs -> let name = (make_str (n::pth)) in
        let convs = convert_mod assocs in
        let nchars = String.concat "," (List.map (fun x -> (printc (CString x))) names)
        and achars = String.concat "," (List.map printa assocs)
        and fchars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") convs ptrs) in
        let nptr = ("char"^(make_var (n::pth)))
        and aptr = ("acc"^(make_var (n::pth)))
        and fptr = ("field"^(make_var (n::pth))) in
        let nls = (printd CHAR)^"* "^nptr^"[] = {"^nchars^"}"
        and als = (printd ACC)^" "^aptr^"[] = {"^achars^"}"
        and fls = (printd FIELD)^" "^fptr^"[] = {"^fchars^"}"
        and cnt = ".count="^(string_of_int (List.length names)) in
        let str = ".c.s={"^(String.concat "," [cnt;".names="^nptr;".accs="^aptr;".fields="^fptr])^"}" in
        let decl = ((printd MODULE)^" "^name^" = {"^  
          (String.concat "," [".mask = "^(string_of_int (-1));".type = STRUCTURE";".stamp = 0"; str])^"}") in
        let setup = [nls ; als ; fls ; decl ] in
        let body = (format 0 setup) in
          (String.concat "\n" (body@[""])) :: (structure xs)
      | _ -> raise (Cannot_compile "print_strcts only prints Strct") 

   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  functor
    *  Description: print a functor method
    * =====================================================================================
    *)
    let rec lambdaf = function [] -> []
      | (Fctr (name,loc,comp)) as f :: xs -> let definition = printf (funcdef false f) 
        and body = (format 1 (computation comp)) in 
        (String.concat "\n" ( (definition::body) @ func_end)) :: (lambdaf xs) 
      | _ -> raise (Cannot_compile "print_fctrs - only compiles Gettr")

  end

end
 
