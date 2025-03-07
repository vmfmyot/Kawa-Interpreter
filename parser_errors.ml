
exception SyntaxError of string

let syntaxerror s = 
    raise (SyntaxError s)
