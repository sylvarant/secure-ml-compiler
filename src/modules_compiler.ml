(*
 * =====================================================================================
 *
 *     Filename: modules_compiler.ml
 *
 *  Description: Compile the Modules into our own representation
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

  and modbindtype = FB of cpath * string * MiniMLMod.mod_term * modbindtype * mask * bool
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
               | Strct of strcttype * string * cpath * string list * Intermediary.accs list * string list * string list 
               | Fctr of  string * Intermediary.locality * computation 
               | Compttr of string * computation * Intermediary.tempc list
  and strcttype = Normal | Copy | Fun | Partial


 (*-----------------------------------------------------------------------------
  *  Helper Functions
  *-----------------------------------------------------------------------------*)

  (* convert a path into a list of strings *)
  let rec convert_path = (function Pident id -> [Ident.name id]
    | Pdot (p,str) -> str :: (convert_path p)) 
  
  (* convert a list into a pointer *)
  let make_entrypoint = function [] -> "this" 
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
    (List.map (function Intermediary.BVAL | Intermediary.BDVAL -> ".gettr = " 
      | Intermediary.BMOD -> ".module = &"
      | Intermediary.BDMOD -> ".mgettr = ") ls)


  (* convert into entry point union *)
  let convert_entry ls =
    (List.map (function 
      | Intermediary.BDVAL -> ".entry_v2 = " 
      | Intermediary.BVAL -> ".entry_v = "
      | Intermediary.BDMOD -> ".entry_m2 = "
      | Intermediary.BMOD -> ".entry_m = ") ls)

 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  lookup_path
  *  Description: Fetches the environent of list of paths
  * =====================================================================================
  *)
  let rec lookup_path env path = 

    (* note that this is not the original one *)
    let set_origin = function 
      | FB (a,b,c,d,e,_) -> FB(a,b,c,d,e,false)
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
        | Dynamic (nn,None) -> Dynamic (nn, Some ls (*List.rev (List.tl (List.rev ls))*))
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

    open Interm  

   (*-----------------------------------------------------------------------------
    *  Helper Funcions
    *-----------------------------------------------------------------------------*)
    let defmask = (-1)


   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  compile
    *  Description:  converts the toplevel into an omega binding
    * =====================================================================================
    *)
    let compile program = 

      (* funct list *)
      let functlist = ref [] in

      (* new functor maks *)
      let new_fctr = let count = ref (0) in
        fun () -> incr count; !count
      in

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
            let fc = new_fctr() in
              FB (pth,(Ident.name id),m,mb,fc,true)
          | Apply (m1,m2) -> 
            (match (parse_module env pth m1) with
              | FB (_,id,m,_,_,_) -> let nenv = (parse_module env pth m2) in
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


      (* new module *)
      let new_module = let count = ref (-1) in
        fun () -> incr count; !count 
      in

      (* fnctr list -- similar to lab *)
      let fctr_list = ref [] in
      let entry_list = ref [] in

      (* quickly extract first level associations and ptrs *)
      let quick ls opt = 
        let entry n pth = match opt with
          | None -> make_entrypoint (n::pth)
          | Some p -> make_entrypoint (n::p)
        in
        let convert = function 
          | BArg  _ -> raise (Cannot_compile_module "Cannot convert BArg")
          | BVal (name,path,_) -> ((name,Interm.BVAL),((make_ptr (name::path)),(entry name path)))
          | BMod (name,path,_) -> ((name,Interm.BMOD),((make_str (name::path)),(entry name path)))
        in
        let (l,r) = (List.split (List.map convert ls)) in
        let (n,a) = (List.split l) in
        let (i,o) = (List.split r) in
        (n,a,i,o)
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
              and (names,assocs,ptrs,entries) = (quick nbinding None)
              and (a,b) = (extract ls) in
              (aa @ a, bb @ (Strct (Normal,name,path,names,assocs,ptrs,entries) :: b))
            | SB (pth,nbinding,false) ->  let (a,b) = (extract ls) 
              and (_,assocs,_,entries) = (quick nbinding (Some (name::path)) ) in
              (a,Strct (Copy,name,path,pth,assocs,[],entries) :: b)
            | FB (pth,var,mm,mb,id,true) -> let npth = (make_ptr pth) in
              let (comp,agls,sls) = (compile_fctr ("Functor"::pth) id mb) 
              (*let cmp =([],[],Interm.ToCall ((Interm.CVar "emptyModule"),[])) *)
              and fcpth = npth ^ "_Fctr" ^ (string_of_int id)
              and (a,b) = (extract ls) in
              fctr_list := (Fctr (fcpth,Interm.LOCAL,comp)) :: !fctr_list;
              (agls@a,Strct(Fun,name,path,fcpth::var::[],[],[],[]) :: sls @ b)
            | FB (pth,var,_,_,_,false) -> let (a,b) = extract ls in 
              let str = (make_str pth) in
              let fcpth = str^".c.f.Functor" in
              (a,Strct(Fun,name,path,fcpth::var::[],[],[],[])::b)))



      (* ================================================= *)
      (* Compile the functor                               *)
      (* ================================================= *)
      and compile_fctr path fctr mb = 

        (* special gettrs *)
        let gettrls = ref [] 
        and strctls = ref [] in

        (* decouple a triple list *)
        let decouple ls = let (l,r) = (List.split ls) in
          let (n,a) = (List.split l) in
          let (i,o) = (List.split r) in
          (n,a,i,o)
        in

        (* make computation default *)
        let make_comp c = ([],[],c) in

        (* The module parser *)
        let rec parse : modbindtype -> computation = function
          | AR (n,None,None) -> make_comp (GetStr ((constv MOD),(CString n)))
          | AR(n,Some ls,None) -> let pp =  (make_path ls) in
            let i = CInt (String.length pp) in
            make_comp (ToCall ((constc PATH),[(constv MOD) ; (CString pp) ; i ]))
          | AR(n,ls,Some mb) -> let (_,set,ar) = (parse (AR(n,ls,None))) 
            and (_,sett,arg) = (parse mb) in
            ([],set@sett,(ToCall(ToFunctor(ar),[arg]))) (* functor call *)
          | FB (pth,var,_,mb,id,true) -> let npth = (make_ptr pth) in
            let (comp,agls,esls) = (compile_fctr ("Functor"::pth) id mb) 
            and fcpth = npth ^ "_Fctr" ^ (string_of_int id) in 
            gettrls := agls @ !gettrls;
            strctls := esls @ !strctls;
            fctr_list := (Fctr (fcpth,Interm.LOCAL,comp)) :: !fctr_list;
            let c = ToCall ((CVar "makeContentF"),[(CVar fcpth);(CString var)]) in
              make_comp (ToCall(CVar "makeModule",[CInt (-1); (CVar "FUNCTOR"); (CInt fctr); (CVar "NULL"); c]))
          | FB (pth,var,_,mb,id,false) -> make_comp (CVar (make_str pth)) 
          | SB (pth,nbinding,true) -> let (names,assocs,ptrs,entries) = decouple (parse_str path nbinding) in
            (* the names list *)
            let nchars = String.concat "," (List.map (fun x -> (printc (CString x))) names) in
            let nptr = ("char"^(make_var pth)) in
            let nls = (printd CHAR)^"* "^nptr^"[] = {"^nchars^"}" in
            (* the accosiations *)
            let achars = String.concat "," (List.map printa assocs) in
            let aptr = ("acc"^(make_var pth)) in
            let als = (printd ACC)^" "^aptr^"[] = {"^achars^"}" in
            (* the Fields *)
            let convs = convert_mod assocs in
            (try
             let fchars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") convs ptrs) in
            let fptr = ("field"^(make_var pth)) in
            let fls = (printd FIELD)^" "^fptr^"[] = {"^fchars^"}" in
            (* The Entrypoints *)
            let eptr = ("entry"^(make_var pth)) in 
            let nstr = Strct (Partial, (List.hd pth),(List.tl pth),[],assocs,[],entries) in
            strctls := nstr ::  !strctls;
            (* update setup *)
            let setup = (CVar nls) :: (CVar als) :: (CVar fls) :: [] in
            (* make the content and module *)
            let args = (List.map (fun x -> CVar x) [nptr;aptr;fptr;eptr]) in
            let c = ToCall (CVar "makeContentS", (CInt (List.length names)::args)) in
             ([],setup,ToCall(CVar "makeModule",[CInt (-1); (CVar "STRUCTURE"); (CInt fctr); (constv MOD); c ]))
            with _ -> raise (Cannot_compile_module "Sure"))
          | SB (pth,nbinding,false) -> make_comp (CVar (make_str pth))
               
        and parse_str path = function [] -> []
          | x :: xs -> match x with
            | BArg _ -> raise (Cannot_compile_module "This shouldn't be here")
            | BVal (name,path, comp) -> let value = (Interm.constd Interm.VALUE) in
              let nn = (make_ptr (name::path)) 
              and en = (make_entrypoint (name::path)) in
              let get = Gettr (nn,value,Interm.ENTRYPOINT,(Some fctr),comp) in
              gettrls := get::!gettrls;
              ((name,Interm.BDVAL),(nn,en)) :: (parse_str path xs)
          | BMod (name,path,modt) ->  let modu = parse modt 
              and ret = (Interm.constd Interm.MODULE)
              and nn = (make_ptr (name::path)) 
              and en = (make_entrypoint (name::path)) in
              let get = Gettr (nn,ret,Interm.ENTRYPOINT,(Some fctr),modu) in
              gettrls := get :: !gettrls;
              ((name,Interm.BDMOD),(nn,en)) :: (parse_str path xs)
        in

        (* toplevel *)
        ((parse mb),!gettrls,!strctls)
      in

      (* toplevel *)
      let (gls,stls) = (extract ls) in 
      let (names,assocs,ptrs,entries) = (quick ls None) in
      let this = Strct(Normal,"this",[],names,assocs,ptrs,entries) in
        (gls,(stls @ [this]),!fctr_list)

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
      | Fctr (ptr,loc,_) -> (loc,(constd MODULE),ptr,[(BINDING,MOD);(MODULE,STR)],b)
      | _ -> raise (Cannot_convert_intermediary "funcdef failed")

    (*(* the check list *)  
    let check_list = ref []

    (* generate the masks *)
    let generate_mask str = let count = ref (-1) in
        fun () -> incr count; 
          check_list := str :: !check_list; 
          !count *)

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
        let setup = env :: [] in
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
      | Strct (Copy,n,pth, p::ps ,assocs,[],entries) :: xs -> let eonvs = (convert_entry assocs) in
        let name = (make_str (n::pth)) 
        and orig_n = "char"^(make_var (p::ps)) 
        and orig_a = "acc"^(make_var (p::ps))
        and orig_f = "field"^(make_var (p::ps))
        and eptr = "entry"^(make_var (n::pth))
        and echars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") eonvs entries) in
        let els = (printd ENTRY)^" "^eptr^"[] = {"^echars^"}" 
        and cnt = ".count="^(string_of_int (List.length assocs)) in
        let arg = [cnt;".names="^orig_n;".accs="^orig_a;".fields="^orig_f;".entries="^eptr] in
        let str = ".c.s={"^(String.concat "," arg )^"}" in
        let decl = ((printd MODULE)^" "^name^" = {"^  
          (String.concat "," [".mask = "^(string_of_int (-1));".type = STRUCTURE";".stamp = -1"; str])^"}") in
        let setup = [els ; decl ] in
        let body = (format 0 setup) in
          (String.concat "\n" (body@[""])) :: (structure xs)
      | Strct (Fun,n,pth,fctr::var::[],[],[],[]) :: xs -> let name = (make_str (n::pth))    
        and fc = ".c.f={.var = \""^var^"\",.Functor ="^fctr^"}" in
        let decl = ((printd MODULE)^" "^name^" = {"^
          (String.concat "," [".mask = "^(string_of_int 2);".type = FUNCTOR";".stamp = -1"; fc])^"}") in
        let body = (format 0 [decl]) in 
          (String.concat "\n" (body@[""])) :: (structure xs)
      | Strct (Partial,n,pth,[],assocs,[],entries) :: xs ->
        let eonvs = (convert_entry assocs) in
        let echars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") eonvs entries) in
        let eptr = ("entry"^(make_var (n::pth))) in
        let els = (printd ENTRY)^" "^eptr^"[] = {"^echars^"}" in
        let setup = [els] in
        let body = (format 0 setup) in
          (String.concat "\n" (body@[""])) :: (structure xs)
      | Strct (Normal,n,pth,names,assocs,ptrs,entries) :: xs -> let name = (make_str (n::pth)) in
        let convs = convert_mod assocs 
        and eonvs = (convert_entry assocs) in
        let nchars = String.concat "," (List.map (fun x -> (printc (CString x))) names)
        and achars = String.concat "," (List.map printa assocs)
        and fchars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") convs ptrs) 
        and echars = String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") eonvs entries) in
        let nptr = ("char"^(make_var (n::pth)))
        and aptr = ("acc"^(make_var (n::pth)))
        and fptr = ("field"^(make_var (n::pth))) 
        and eptr = ("entry"^(make_var (n::pth))) in
        let nls = (printd CHAR)^"* "^nptr^"[] = {"^nchars^"}"
        and als = (printd ACC)^" "^aptr^"[] = {"^achars^"}"
        and fls = (printd FIELD)^" "^fptr^"[] = {"^fchars^"}"
        and els = (printd ENTRY)^" "^eptr^"[] = {"^echars^"}"
        and cnt = ".count="^(string_of_int (List.length names)) in
        let arg = [cnt;".names="^nptr;".accs="^aptr;".fields="^fptr;".entries="^eptr] in
        let str = ".c.s={"^(String.concat "," arg )^"}" in
        let decl = ((printd MODULE)^" "^name^" = {"^  
          (String.concat "," [".mask = "^(string_of_int (-1));".type = STRUCTURE";".stamp = -1"; str])^"}") in
        let setup = [nls ; als ; fls ; els ; decl ] in
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
      | (Fctr (name,loc,comp)) as f :: xs -> 
        (* parse the entry list 
        let rec parse = function [] -> []
          | (a::(ptr::b)::ls) -> (ptr,a,b) :: (parse ls)
        in
        let collection = (parse els) in
        let entry = (List.map (function (a,b,c) -> 
          let content = (String.concat "," (List.map2 (fun x y -> "{"^x^y^"}") b c)) in 
          (printd ENTRY)^" "^a^"[] ={"^content^"}" ) collection) 
        in *)
        let definition = printf (funcdef false f) 
        and body = (format 1 (computation comp)) in 
        (String.concat "\n" ((definition :: body) @ func_end)) :: (lambdaf xs) 
      | _ -> raise (Cannot_compile "print_fctrs - only compiles Gettr")

  end

end
 
