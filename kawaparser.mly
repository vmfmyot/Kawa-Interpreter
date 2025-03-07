%{

  open Lexing
  open Kawa
  open Parser_errors

%}

%token <string> STRING
%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token <char> CHAR
%token OPP NOT
%token TRUE FALSE
%token VOID
%token MAIN
%token LPAR RPAR BEGIN END SEMI LBRA RBRA
%token IF WHILE ELSE FOR
%token CLASS NEW THIS RETURN INT_T BOOL_T STRING_T CHAR_T INSTANCEOF
%token VIR DOT EXTENDS SUPER DPOINTS
%token ADD SUB MUL DIV REM CONCAT
%token EQ NEQ EQSTRUCT NEQSTRUCT LT LE GT GE AND OR
%token SET
%token PRINT
%token EOF

%left  AND OR CONCAT
%left  EQ NEQ LT LE GT GE INSTANCEOF
%left  EQSTRUCT NEQSTRUCT
%left  ADD SUB
%left  MUL DIV REM 
%left  OPP NOT IDENT 
%right DOT 
%left  RPAR 

%start program
%type <Kawa.program> program


%%

program:
| v = def_typ* c = list(class_def) MAIN BEGIN main = list(instruction) END EOF
    { {classes=c; globals=List.flatten(List.filter_map (fun x -> match x with VarAttr v -> Some v | _ -> None) v); main} }
//errors
| v = def_typ* c = list(class_def) MAIN error
    { ignore(v,c); syntaxerror ("missing '{'") }
| v = def_typ* c = list(class_def) MAIN BEGIN main = list(instruction) error
    { ignore(v,c); syntaxerror ("missing '}'") }
;

class_def:
| CLASS nom = IDENT BEGIN attrMeth = def_typ* END
    { { class_name = nom; attributes=List.flatten(List.filter_map (fun x -> match x with VarAttr v -> Some v | _ -> None) attrMeth);
     methods=List.filter_map (fun x -> match x with Meth m -> Some m | _ -> None) attrMeth; parent = None } }

| CLASS nom = IDENT EXTENDS parent = IDENT BEGIN attrMeth = def_typ* END
    { { class_name = nom; attributes=List.flatten(List.filter_map (fun x -> match x with VarAttr v -> Some v | _ -> None) attrMeth);
     methods=List.filter_map (fun x -> match x with Meth m -> Some m | _ -> None) attrMeth; parent = Some(parent) } }
//errors
|  CLASS nom = IDENT error {ignore(nom); syntaxerror ("missing '{'")}
|  CLASS nom = IDENT EXTENDS parent = IDENT error {ignore(nom, parent); syntaxerror ("missing '{'")}
|  CLASS nom = IDENT BEGIN attrMeth = def_typ* error
    { ignore(nom, attrMeth); syntaxerror ("missing '}'") }
| CLASS nom = IDENT EXTENDS parent = IDENT BEGIN attrMeth = def_typ* error
    { ignore(nom, parent, attrMeth); syntaxerror ("missing '}'") }
;

def_typ: 
| m = method_def { Meth(m) }
| v = var SEMI   { VarAttr(v) }
;
 
method_def: 
| tn = meth_name LPAR p = separated_list(VIR,param) RPAR 
    BEGIN code = list(instruction) END 
    { {method_name = (snd tn); code;params=p; return = (fst tn) } }
//errors
| tn = meth_name LPAR p = separated_list(VIR,param) RPAR error
    { ignore(tn, p); syntaxerror ("missing '{") }
| tn = meth_name LPAR p = separated_list(VIR,param) RPAR 
    BEGIN code = list(instruction) error
    { ignore(tn, p, code); syntaxerror ("missing '}") }
;

meth_name :
| t = typ name = IDENT {(t, name)}
| VOID name = IDENT {(TVoid, name)}
;

param: 
| t = typ n = IDENT { (n, t) }
;

instruction:
| i = instr_semi SEMI { i }
| IF LPAR e = expression RPAR
    BEGIN i1 = list(instruction) END
    ELSE BEGIN i2 = list(instruction) END
    { If(e, i1, i2) }
| WHILE e = expression BEGIN i = list(instruction) END { While(e, i) }
| FOR LPAR t = typ n = IDENT SET v = expression SEMI e1 = expression 
     SEMI e2 = expression RPAR BEGIN il = list(instruction) END 
         { For((n, t, v),e1,e2,il) }
| FOR LPAR t = typ n = IDENT DPOINTS i = IDENT RPAR BEGIN il = list(instruction) END 
         { ForTab((n, t), i, il) }
| v = def_typ { match v with VarAttr a -> Lvar(a) | _ -> syntaxerror ("expected variable declaration") }
//errors
| i = instr_semi error { ignore(i); syntaxerror ("missing semicolon") }
| IF LPAR e = expression RPAR error | WHILE e = expression error 
    { ignore(e); syntaxerror ("missing '{'") }
| WHILE e = expression BEGIN i1 = list(instruction) error 
| IF LPAR e = expression RPAR BEGIN i1 = list(instruction) error 
    {ignore(e,i1); syntaxerror ("missing '}'") }
| IF LPAR e = expression RPAR BEGIN i1 = list(instruction) END
    ELSE error { ignore(e,i1); syntaxerror ("missing '{'") }
| IF LPAR e = expression RPAR BEGIN i1 = list(instruction) END
    ELSE BEGIN i2 = list(instruction) error 
    {ignore(If(e, i1, i2)); syntaxerror ("missing '}'") }
;

%inline instr_semi : 
| RETURN e = expression { Return(e) }
| PRINT LPAR e=expression RPAR { Print(e) }
| m = mem SET e=expression { Set(m, e) }
| e = expression { Expr(e) }
; 
 
var : 
| t = typ i = separated_nonempty_list(VIR, ident) { (List.map (fun (i, o) -> (i, t, o)) i) }
| t = typ LBRA RBRA i = separated_nonempty_list(VIR, ident) { (List.map (fun (i, o) -> (i, TTab(t), o)) i) }
//errors
| t = typ i = separated_nonempty_list(VIR, ident) error
| t = typ LBRA RBRA i = separated_nonempty_list(VIR, ident) error { ignore(t, i); syntaxerror ("missing semicolon") }
;

ident:
| i = IDENT { (i, None) }
| i = IDENT SET e = expression { (i, Some e) }
;

expression:
| n = INT { Int(n) }
| b = BOOL { Bool(b) }
| s = STRING { String(s) }
| c = CHAR { Char(c) }
| m = mem { Get(m) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS { This }
| u = uop e = expression { Unop(u, e) } 
| LPAR e=expression RPAR { e }
| NEW s = IDENT LPAR l = separated_list(VIR, expression) RPAR { NewCstr(s, l) }
| NEW s = IDENT { New(s) }
| e1 = expression b = bop  e2 = expression { Binop(b, e1, e2) }
| e = expression b = bop b1 = bop 
    { assert (b1 = b); match b with Add | Sub -> Binop(b, e, Int(1)) | _ -> Binop(b, e, e) }
| e = expression DOT i = IDENT LPAR l = separated_list(VIR, expression) RPAR { MethCall(e, i, l) }
| e = expression INSTANCEOF c = IDENT { Instanceof(e, TClass(c)) }
| LPAR c = IDENT RPAR e = expression { Transtype(TClass(c), e) }
| SUPER LPAR l = separated_list(VIR, expression) RPAR {Super(l)}
| SUPER DOT s = IDENT LPAR l = separated_list(VIR, expression) RPAR { SuperCstr(s, l) }
| NEW t = typ LBRA e = expression? RBRA { NewTab(t, e) }
| LBRA e = separated_list(VIR, expression) RBRA { TabVal(e) }
//errors
| LPAR e = expression error {syntaxerror ("unclosed parenthesis")}
;

mem:
| n = IDENT { Var(n) }
| e = expression DOT s = IDENT { Field(e, s) }
| n = IDENT LBRA e = expression RBRA { TabAccess(n, e) }
;

%inline typ:
| INT_T       { TInt }
| BOOL_T      { TBool }
| c = IDENT   { TClass(c) }
| STRING_T    { TString }
| CHAR_T      { TChar }
;
%inline bop : 
| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
| REM { Rem }
| EQ  { Eq  }
| NEQ { Neq }
| EQSTRUCT  { Eqstruct  }
| NEQSTRUCT { Neqstruct }
| LT  { Lt  }
| LE  { Le  }
| GT  { Gt  }
| GE  { Ge  }
| AND { And }
| OR  { Or  }
| CONCAT { Concat }
;
%inline uop :
| OPP { Opp }
| NOT { Not }
;
