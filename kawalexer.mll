{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 22 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",      PRINT;
      "main",       MAIN; 
      "class",      CLASS;
      "new",        NEW; 
      "this",       THIS;
      "if",         IF;
      "else",       ELSE;
      "while",      WHILE;
      "for",        FOR;
      "return",     RETURN;
      "void",       VOID;
      "extends",    EXTENDS;
      "int",        INT_T;
      "bool",       BOOL_T;
      "string",     STRING_T;
      "instanceof", INSTANCEOF;
      "not",        NOT;
      "true",       TRUE;
      "false",      FALSE;
      "super",      SUPER;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z''A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
let tout = [^'"']
let string = "\""(("\\\"")|tout)*"\""
let char = "'"[^'\n']?"'"
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }
  | string as s  { let str = String.sub s 1 ((String.length s) - 2) in STRING(str) }
  | char as c    { CHAR(c.[1]) }
  
  | ";"  { SEMI }
  | "," { VIR }
  | "." { DOT }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "["  { LBRA }
  | "]"  { RBRA }
  | "{"  { BEGIN }
  | "}"  { END }
  | "="  { SET }
  | "==" { EQ }
  | "!=" { NEQ }
  | "===" { EQSTRUCT }
  | "=/=" { NEQSTRUCT }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { REM }
  | "<" { LT }
  | "<=" { LE }
  | ">"  { GT }
  | ">=" { GE }
  | "&&" { AND }
  | "||" { OR }
  | "-"  { OPP }
  | "@"  { CONCAT }
  | ":"  { DPOINTS }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
