(*
 * =====================================================================================
 *
 *     Filename: modules_compiler.ml
 *
 *  Description: Compile the Modules into our own representation
 *
 *     Author:  MYSTERY MAN, 
 *    Company:  SOMEWHERE IT
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
exception FailSort 


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

  type modinfo = bool * bool

  and modbindtype = FB of cpath * string * strctbinding list * MiniMLMod.mod_term * modbindtype * mask * modinfo
                  | SB of cpath * strctbinding list * modinfo
                  | AR of string * string list option * modbindtype option * mask

  and strctbinding = BVal of string * cpath * computation 
                   | BMod of string * cpath * modbindtype 
                   | BArg of string  (* functor argument *)

  and computation = Intermediary.tempc list * Intermediary.tempc list * Intermediary.tempc 

  (* the results of look ups into the omega environement *)
  and trawl = Static of string 
            | Environment of modbindtype
            | Dynamic of string * string list option

  and omega = strctbinding list
  
  type entry = string option

  type grec = { path : cpath; retty : Intermediary.datastr; locality : Intermediary.locality; 
                mask : mask option; comp : computation}

  type compred = Gettr of grec
               | Strct of strcttype * string * cpath * string list * Intermediary.accs list * string list * entry list 
               | Fctr of  string * Intermediary.locality * computation 
               | Compttr of string * computation * Intermediary.tempc list

  and strcttype = Normal | Copy | Fun | Partial


 (*-----------------------------------------------------------------------------
  *  Helper Functions
  *-----------------------------------------------------------------------------*)

  (* convert a path into a list of strings *)
  let rec convert_path = (function Pident id -> [Ident.name id]
    | Pdot (p,str) -> str :: (convert_path p)) 
  
  (* convert a list into an entry point *)
  let make_entrypoint = function [] -> None
    | lst -> Some (String.concat "_" (List.rev lst)) 

  (* helper for ptrs *)
  let make_baseptr = function [] -> "this"
    | lst -> (String.concat "_" (List.rev lst))

  (* convert a list into a standard ptr *)
  let make_ptr lst = "_" ^ (make_baseptr lst)

  (* convert a list into a standard ptr *)
  let make_vptr lst = "v_" ^ (make_baseptr lst)

  (* convert a list into a structure ptr *)
  let make_str lst = "str" ^ (make_ptr lst)

  (* convert a list into a variable ptr *)
  let make_var lst = "var"^ (make_ptr lst)

  (* convert list into a an entry point ptr *)
  let filter_fctr = (fun x -> if x = "Functor" then false else true) 
  let make_eptr lst = "entry_"^(make_ptr (List.filter filter_fctr lst))

  (* produce ptr to list of entry variables *)
  let make_ieptr lst = "ie_"^(make_ptr (List.filter filter_fctr lst))

  (* the difference between a ptr and a path is the path is the . *)
  let make_path = function [] -> ""
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
      | FB (a,b,x,c,d,e,(z,_)) -> FB(a,b,x,c,d,e,(z,false))
      | SB (a,b,(z,_)) -> SB(a,b,(z,false))
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
      with Not_found -> (*(Printf.eprintf "Missed %s\n" x);*) 
        raise (Omega_miss "Identifier not found") 
    in

    (* When it comes to values all that matters is that they exist *)
    let extract = function
        | BVal (nn,pth,_) -> Static (make_ptr (nn::pth))
        | BMod (_,_,e) -> (Environment (set_origin e)) 
        | BArg nn -> (Dynamic (nn,None))
    in

    (* top level *)
    (*Printf.eprintf "full path = %s\n" (String.concat "." path);*)

    (* toplevel *)
    match path with  | [] -> raise (Omega_miss "Empty path given to lookup")
      | x::[] -> (extract (get_binding x env)) 
      | x::xs as ls -> match (lookup_path env xs) with
        | Environment( SB (_,nenv,_)) -> (lookup_path nenv (x::[]))
        | Dynamic (nn,None) -> Dynamic (nn, Some ls (*List.rev (List.tl (List.rev ls))*))
        | Environment ( AR(nn,l,_,_) ) -> 
          let extra = (match l with (* TODO what about appl ? *)
          | None -> [nn]
          | Some lst -> (nn::lst)) in 
          Dynamic(nn,Some (List.rev (extra@ (List.rev (List.tl (List.rev ls))))))
        | _ -> raise (Cannot_compile "Wrong tree structure") 


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  sort_compred
  *  Description: sort the compiler redices
  * =====================================================================================
  *)
  let sort_compred cls =
    let cmp_red a b = match (a,b) with
      | (Gettr {path = p},Gettr {path = p2}) -> (String.compare (make_ptr p) (make_ptr p2))
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


 (* 
  * ===  FUNCTION  ======================================================================
  *     Name:  sort_bindings
  *  Description: sort the structure bindings
  * =====================================================================================
  *)
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

    (* create fctr id's *)
    let new_fctr = let count = ref (0) in
      fun () -> incr count; !count

   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  compile
    *  Description:  converts the toplevel into an omega binding
    * =====================================================================================
    *)
    let compile program = 

      (* funct list *)
      let functlist = ref [] in

      (* convert a sequence of structure definitions *)
      let rec parse_struct env path dyn strctls = 

        (* convert a module definition into a new environment *)
        let rec parse_module env pth dyn = function
            Longident ident -> (match (lookup_path env (convert_path ident)) with 
              | (Environment e) -> e
              | Dynamic (n,ls) -> let fc = new_fctr() in AR (n,ls,None,fc)
              | _ -> raise (Cannot_compile_module "Did not retrieve environment from path lookup"))
          | Structure strls -> let parsed = (parse_struct env pth dyn strls) in SB (pth,parsed,(dyn,true))
          | Functor (id,ty,m) -> (*Printf.eprintf "Fun\n";*) let bind = BArg (Ident.name id) in
            let mb = (parse_module (bind::env) ("Functor"::pth) true m) in (* compile functor cont with an arg *)
            let fc = new_fctr() in
              FB (pth,(Ident.name id),env,m,mb,fc,(dyn,true))
          | Apply (m1,m2) -> (*Printf.eprintf "Once\n";*)
            (match (parse_module env pth dyn m1) with
              | FB (_,id,e,m,_,_,_) -> let nenv = (parse_module env pth dyn m2) in
                (*(Printf.eprintf "Added %s for %d \n" id (List.length e));*)
                (parse_module ((BMod (id,pth,nenv))::e) pth dyn m)
              | AR (n,ls,None,_) -> let nm = (parse_module env pth dyn m2) in 
                let fc = new_fctr() in
                AR (n,ls, (Some nm),fc)
              | _ -> raise (Cannot_compile_module "Needed Functor or AR hole"))
          | Constraint (m,ty) -> (parse_module env pth dyn m) 
        in

        (* recurse over the list of definitions *)
        match strctls with [] -> []
          | x::xs ->  match x with 
              Type_str (id2,_,_) -> (parse_struct env path dyn xs)
            | Module_str (id2,mterm) ->
              let name = (Ident.name id2) in
              let nenv = (parse_module env (name::path) dyn mterm) in
              let value = BMod (name,path,nenv) in
              value :: (parse_struct (value :: env) path dyn xs)
            | Value_str (id2,term) ->
              let name = (Ident.name id2) in
              let (flist,comp) = (ExprComp.compile env (name::path) term) in 
              functlist := flist @ !functlist;
              let data = BVal (name, path,comp) in 
                data :: (parse_struct (data :: env) path dyn xs) 
      in
        
      (* top level *)
      match program with Structure strt -> (!functlist,(parse_struct [] [] false strt))
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
          | BVal (name,path,body) -> let (a,b) = (extract ls) 
            and record = { 
                path = (name::path); 
                retty = Interm.VALUE;   
                locality = Interm.ENTRYPOINT;
                mask = None;
                comp = body;
            } in
            ((Gettr record) :: a, b)
          | BMod (name,path, modt) -> (match modt with
            | AR _ -> raise (Cannot_compile_module "Cannot extract AR")
            | SB (_,nbinding,(_,true)) ->  let (aa,bb) = (extract nbinding)  
              and (names,assocs,ptrs,entries) = (quick nbinding None)
              and (a,b) = (extract ls) in
              (aa @ a, bb @ (Strct (Normal,name,path,names,assocs,ptrs,entries) :: b))
            | SB (pth,nbinding,(_,false)) ->  let (a,b) = (extract ls) 
              and (_,assocs,_,entries) = (quick nbinding (Some (name::path)) ) in
              (a,Strct (Copy,name,path,pth,assocs,[],entries) :: b)
            | FB (pth,var,_,_,mb,id,(_,true)) -> let npth = (make_ptr pth) in
              let (comp,agls,sls) = (compile_fctr ("Functor"::pth) id var mb) 
              (*let cmp =([],[],Interm.ToCall ((Interm.CVar "emptyModule"),[])) *)
              and fcpth = npth ^ "_Fctr" ^ (string_of_int id)
              and (a,b) = (extract ls) in
              fctr_list := (Fctr (fcpth,Interm.ENTRYPOINT,comp)) :: !fctr_list;
              (agls@a,Strct(Fun,name,path,fcpth::var::(string_of_int id)::[],[],[],[]) :: sls @ b)
            | FB (pth,var,_,_,_,id,(_,false)) -> let (a,b) = extract ls in 
              let str = (make_str pth) in
              let fcpth = str^".c.f.Functor" in
              (a,Strct(Fun,name,path,fcpth::var::(string_of_int id)::[],[],[],[])::b)))



      (* ================================================= *)
      (* Compile the functor                               *)
      (* ================================================= *)
      and compile_fctr path fctr fvar mb = 

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

        let up c id = UpdateB (c,id,constv MOD) in

        (* The module parser *)
        let rec parse : modbindtype -> computation = function
          | AR (n,None,None,id) -> make_comp (up (GetStr ((constv MOD),(CString n))) id)
          | AR(n,Some ls,None,id) -> let pp =  (make_path ls) in
            make_comp (up (ToCall ((constc PATH),[(constv MOD) ; (CString pp)])) id)
          | AR(n,ls,Some mb,id) -> let (_,set,ar) = (parse (AR(n,ls,None,id))) 
            and (_,sett,arg) = (parse mb) in
            ([],set@sett,(up (ToCall(ToFunctor(ar),[(constv MOD);arg])) id)) (* functor call *)
          | FB (pth,var,_,_,mb,id,(_,true)) -> let npth = (make_ptr pth) in
            let (comp,agls,esls) = (compile_fctr ("Functor"::pth) id var mb) 
            and fcpth = npth ^ "_Fctr" ^ (string_of_int id) in 
            gettrls := agls @ !gettrls;
            strctls := esls @ !strctls;
            fctr_list := (Fctr (fcpth,Interm.LOCAL,comp)) :: !fctr_list;
            let c = ToCall ((CVar "makeContentF"),[(CVar fcpth)]) in
              make_comp (ToCall(CVar "makeModule",[(CVar "FUNCTOR"); (constv MOD); c]))
          | FB (pth,var,_,_,mb,id,(dyn,false)) -> let nf = new_fctr() in
            let target =  if not dyn 
            then up (CVar (make_str pth)) nf
            else up (ToCall ( (CVar (make_ptr pth)), [constv MOD])) nf in
              (make_comp target)
          | SB (pth,nbinding,(_,true)) -> let (names,assocs,ptrs,entries) = decouple (parse_str path nbinding) in
            (* the names list *)
            let nchars = String.concat "," (List.map (fun x -> (printc (CString x))) names) in
            let nptr = ("char"^(make_var pth)) in
            let nls = (printd CHAR)^" "^nptr^"[] = {"^nchars^"}" in
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
              let eptr = (make_eptr pth) in 
              let ieptr = (make_ieptr pth) in
              let nstr = Strct (Partial, (List.hd pth),(List.tl pth),[],assocs,[],entries) in
              strctls := nstr ::  !strctls;
              (* update setup *)
              let setup = (CVar nls) :: (CVar als) :: (CVar fls) :: [] in
              (* make the content and module *)
              let args = (List.map (fun x -> CVar x) [nptr;aptr;fptr;ieptr;eptr]) in
              let c = ToCall (CVar "makeContentS", (CInt (List.length names)::args)) in
               ([],setup,ToCall(CVar "makeModule",[(CVar "STRUCTURE"); (constv MOD); c ]))
             with _ -> raise (Cannot_compile_module "Sure"))
          | SB (pth,nbinding,(dyn,false)) -> let target = if not dyn 
            then (CVar (make_str pth))
            else ToCall ( (CVar (make_ptr pth)), [constv MOD]) in
              (make_comp target)
               
        and parse_str path = function [] -> []
          | x :: xs -> match x with
            | BArg _ -> raise (Cannot_compile_module "This shouldn't be here")
            | BVal (name,path,body) -> let nn = (make_ptr (name::path)) 
              and en = (make_entrypoint (name::path)) in
              let record = {
                path = (name::path); 
                retty = Interm.VALUE;
                locality = Interm.ENTRYPOINT;
                mask = (Some fctr);
                comp = body;
              } in
              gettrls := (Gettr record)::!gettrls;
              ((name,Interm.BDVAL),(nn,en)) :: (parse_str path xs)
          | BMod (name,path,modt) ->  let modu = parse modt 
              and nn = (make_ptr (name::path)) 
              and en = (make_entrypoint (name::path)) in
              let record = {
                path = (name::path);
                retty = Interm.MODULE;
                locality = Interm.ENTRYPOINT;
                mask = Some fctr;
                comp = modu;
              } in
              gettrls := (Gettr record) :: !gettrls;
              ((name,Interm.BDMOD),(nn,en)) :: (parse_str path xs)
        in

        (* toplevel *)
        let var = var_prefix^"arg"  in
        let set = MALLOC(MODULE,CVar var,Sizeof MODULE) 
        and ass = Assign(Ptr (CVar var), (constv STR)) 
        and ins = Insert ((constv MOD),CString fvar,(CVar var))
        and (a,b,c) = (parse mb) in
        ((a,set :: ass :: ins :: b,c), !gettrls,!strctls)
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
    let rec computation boolean data = function (vlist,sls,comp) -> 
      let c = if boolean 
        then ["static int _dec = 0";"static "^data^" _rValue";("SWITCH(_dec,_rValue,"^(printc comp)^")");"return _rValue"] 
        else ["return "^(printc comp)]
      in
      let l = match sls with [] -> []
        | _ -> (List.map printc sls)
      in
      let v = match vlist with [] -> []
        | _ -> [(printd VALUE)^ " " ^(String.concat "," (List.map printc vlist))] 
      in
        v@l@c 

    (* build funcdefi *) 
    let funcdef b = function
      | Gettr record -> (record.locality,(Interm.constd record.retty),(make_ptr record.path),[(BINDING,MOD)],b)
      | Fctr (ptr,loc,_) -> (loc,(constd MODULE),ptr,[(BINDING,MOD);(MODULE,STR)],b)
      | _ -> raise (Cannot_convert_intermediary "funcdef failed")

    (* map the entry points *)
    let map_entries assocs eptrs = 
        let rec filter2 l1 l2 = match l1 with
          | [] -> []
          | x :: xs -> match l2 with
            | None :: ys -> (filter2 xs ys) 
            | Some y :: ys -> ("{"^x^y^"}") :: (filter2 xs ys)
            | _ -> raise (Cannot_compile_module "Incorrectly formatted double list")
        in
        let ie = (List.map (function None -> "NO" | _ -> "YES") eptrs) in
        let final = match (filter2 (convert_entry assocs) eptrs) with
         | [] -> ["NULL"]
         | ls -> ls in
        (ie,final)

    let print_ls ty ptr ls = (printd ty)^" "^ptr^"[] = {"^(String.concat "," ls)^"}"

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
          let body = (format 1 (setupls @ (computation false (printd VALUE) comp))) in
          (String.concat "\n" (((printf definition)::body) @ func_end)) :: (lambda xs) 
      | _ -> raise (Cannot_convert_intermediary "print_lambdas - only compiles Compttr")

   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  getter
    *  Description: print a getter
    * =====================================================================================
    *)
    let rec getter = function [] -> []
      | Gettr record :: xs -> 
        let args = [ (BINDING,MOD) ] in
        let definition = (record.locality,(Interm.constd record.retty),(make_ptr record.path),args,false) in
        let env = (printd BINDING)^" "^(printconst ENV)^" = NULL" 
        (*and md = (printd BINDING)^" "^(printconst MOD)^" = "^(printconst MOD)*) in
        let setup = env :: [] in
        let static = (match record.mask with Some _ -> false | _ -> true) in
        let body = (format 1 (setup @ (computation static (Interm.printd record.retty) record.comp))) in
        (String.concat "\n" ( ((printf definition)::body) @ func_end ) ) :: (getter xs) 
      | _ -> raise (Cannot_convert_intermediary "print_getters - only compiles Gettr")


   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  load
    *  Description: print the load function
    * =====================================================================================
    *)
    let rec load lst = 
      let definition = (Interm.ENTRYPOINT,(Interm.constd Interm.VOID),"load",[],false) in
      let calls = let rec convert = function [] -> []
        | Gettr {path = p; retty = Interm.VALUE; mask = None} :: xs -> 
          let y = ToCall((CVar (make_ptr p)),[CVar "NULL"]) in 
          (y :: (convert xs))
        | _ :: xs -> (convert xs) in 
        (convert lst) in
      let body = (format 1 (List.map printc calls)) in
      (String.concat "\n" (((printf definition) :: body) @ func_end))

   (* 
    * ===  FUNCTION  ======================================================================
    *     Name:  structure
    *  Description: print a structure
    * =====================================================================================
    *)
    let rec structure : compred list -> string list  = function [] -> []
      | Strct (Copy,n,pth, p::ps ,assocs,[],entries) :: xs -> 
        let name = (make_str (n::pth)) 
        and orig_n = "char"^(make_var (p::ps)) 
        and orig_a = "acc"^(make_var (p::ps))
        and orig_f = "field"^(make_var (p::ps))
        and ieptr = "ie"^(make_var (n::pth))
        and eptr = (make_eptr (n::pth))
        and (iechars,echars) = (map_entries assocs entries) in
        let els = print_ls ENTRY eptr echars 
        and iels = print_ls ISENTRY ieptr iechars
        and cnt = ".count="^(string_of_int (List.length assocs)) in
        let arg = [cnt;".names="^orig_n;".accs="^orig_a;".fields="^orig_f;".ie="^ieptr;".entries="^eptr] in
        let str = ".c.s={"^(String.concat "," arg )^"}" in
        let decl = ((printd MODULE)^" "^name^" = {"^  
          (String.concat "," [".type = STRUCTURE";".strls = NULL";".keys=NULL";str])^"}") in
        let setup = [iels;els ; decl ] in
        let body = (format 0 setup) in
          (String.concat "\n" (body@[""])) :: (structure xs)
      | Strct (Fun,n,pth,fctr::var::id::[],assocs,names,entries) :: xs -> 
        let (setup,cnt,nptr,eptr) = (match names with 
          | [] -> ([],"-1","NULL","NULL")
          | _ -> 
            let nchars = (List.map (fun x -> (printc (CString x))) names)
            and (_,echars) = (map_entries assocs entries) in
            let nptr = ("char"^(make_var (n::pth)))
            and eptr = (make_eptr (n::pth)) in
            let nls = print_ls CHAR nptr nchars
            and els = print_ls ENTRY eptr echars
            and cnt = (string_of_int (List.length names)) in
            ([nls;els],cnt,nptr,eptr)) 
        in
        let targets = ["Functor";"count";"names";"entries"] in
        let values = [fctr;cnt;nptr;eptr] in
        let name = (make_str (n::pth))    
        and fc = ".c.f={"^(String.concat "," (List.map2 (fun x y -> "."^x^"="^y) targets values))^"}" in 
        let decl = ((printd MODULE)^" "^name^" = {"^
          (String.concat "," [".type = FUNCTOR";".strls = NULL"; ".keys = NULL"; fc])^"}") in
        let body = (format 0 (setup @ [decl])) in 
          (String.concat "\n" (body@[""])) :: (structure xs)
      | Strct (Partial,n,pth,[],assocs,[],entries) :: xs ->
        let eonvs = (convert_entry assocs) in
        let (iechars,echars) = map_entries assocs entries in
        let ieptr = (make_ieptr (n::pth)) in
        let eptr = (make_eptr (n::pth)) in
        let els = print_ls ENTRY eptr echars in
        let iels = print_ls ISENTRY ieptr iechars in
        let setup = [iels;els] in
        let body = (format 0 setup) in
          (String.concat "\n" (body@[""])) :: (structure xs)
      | Strct (Normal,n,pth,names,assocs,ptrs,entries) :: xs -> let name = (make_str (n::pth)) in
        let convs = convert_mod assocs in
        let nchars = (List.map (fun x -> (printc (CString x))) names)
        and achars = (List.map printa assocs)
        and fchars = (List.map2 (fun x y -> "{"^x^y^"}") convs ptrs) 
        and (iechars,echars) = (map_entries assocs entries) in
        let nptr = ("char"^(make_var (n::pth)))
        and aptr = ("acc"^(make_var (n::pth)))
        and fptr = ("field"^(make_var (n::pth))) 
        and ieptr = "ie"^(make_var (n::pth)) 
        and eptr = (make_eptr (n::pth)) in
        let nls = print_ls CHAR nptr nchars
        and als = print_ls ACC aptr achars
        and ils = print_ls ISENTRY ieptr iechars
        and fls = print_ls FIELD fptr fchars
        and els = print_ls ENTRY eptr echars
        and cnt = ".count="^(string_of_int (List.length names)) in
        let arg = [cnt;".names="^nptr;".accs="^aptr;".fields="^fptr;".ie="^ieptr;".entries="^eptr] in
        let str = ".c.s={"^(String.concat "," arg )^"}" in
        let decl = ((printd MODULE)^" "^name^" = {"^  
          (String.concat "," [".type = STRUCTURE";".strls = NULL";".keys=NULL";str])^"}") in
        let setup = [nls ; als ; fls ; ils; els ; decl ] in
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
        and body = (format 1 (computation false (printd MODULE) comp)) in 
        (String.concat "\n" ((definition :: body) @ func_end)) :: (lambdaf xs) 
      | _ -> raise (Cannot_compile "print_fctrs - only compiles Gettr")

  end

end
 
