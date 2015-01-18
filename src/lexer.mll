(*
 * =====================================================================================
 *
 *       Filename:  lexer.mll
 *
 *    Description:  MiniML ML lexer based on the one by Leroy for miniML
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  uppsala
 *
 * =====================================================================================
 *)

{

exception Lexical_error of string

open Parser    

(* The table of keywords and infixes *)

let keyword_table = (Hashtbl.create 17 : (string, token) Hashtbl.t)

let _ = List.iter (fun (str,tok) -> Hashtbl.add keyword_table str tok)
 [

(* types *)

  "bool", TBOOL;
  "int", TINT;
  "Unit", TUNIT;
  "sig", SIG;

(* terms *)
  "unit", UNIT;
  "true", TRUE;
  "false", FALSE;
  "function", FUNCTION;
  "fun", FUNCTION;
  "let", LET;
  "struct", STRUCT;
  "end", END;
  "functor", FUNCTOR;
  "val", VALUE;
  "type", TYPE;
  "module", MODULE;
  "open", OPEN;
  "ref", REF;
  "in", IN;
  "if", IF;
  "fst", FST;
  "snd", SND;
  "then", THEN;
  "else", ELSE]

(* End of auxiliary Caml definitions *)
}

(* Lexer rules *)

rule token = parse
    [ ' ' '\010' '\013' '\009' '\012' ] +
            { token lexbuf }
  | [ (*'A'-'Z'*) 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\254' ]
    [ 'A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\254'
      '0'-'9' '_' '\'' ] *
            { let s = Lexing.lexeme lexbuf in
              try
                Hashtbl.find keyword_table s
              with Not_found ->
                IDENT s }

  | [ 'A'-'Z' ]
    [ 'A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\254'
      '0'-'9' '_' '\'' ] *
            { let s = Lexing.lexeme lexbuf in
              try
                Hashtbl.find keyword_table s
              with Not_found ->
                MODIDENT s } 
  
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
            { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "(*"
            { comment lexbuf; token lexbuf }
  
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "!"     { EXCLAMATION }
  | "."     { DOT }
  | ";"     { SEMICOLON }
  | ";;"    { SEMISEMI }
  | "->"    { ARROW }
  | "="     { EQUAL }
  | ":="    { DOTEQUAL }
  | ","     { COMMA }
  | "'"     { QUOTE }
  | ":"     { COLON }
  | "*"     { STAR }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "/"     { SLASH }
  | "=="    { EQUALEQUAL }
  | "<>"    { LESSGREATER }
  | "<"     { LESS }
  | ">"     { GREATER }
  | "<="    { LESSEQUAL }
  | ">="    { GREATEREQUAL }
  | eof     { EOF }

  | _       { raise(Lexical_error "illegal character") }

and comment = parse
    "*)"
            { () }
  | "(*"
            { comment lexbuf; comment lexbuf }
  | eof
            { raise (Lexical_error "comment not terminated") }
  | _
            { comment lexbuf }

