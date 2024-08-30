{

  open Parser
  exception Eof
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+
let booleen = "true" | "false"
let lettre_minuscule = ['a'-'z']
let lettre_majuscule = ['A'-'Z']
let lettre = lettre_minuscule | lettre_majuscule
                         
rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']        { token lexbuf }
    (* on saute les blancs et les tabulations en faisant cet appel récursif à "token" *)
  | '+'                    { PLUS }
  | '*'                    { TIMES }
  | '-'                    { MINUS }
  | '/'                    { DIV }
  | '='                    { EQUAL }
  | '>'                    { GREATER }
  | '<'                    { LESS }
  | nombre as s            { INT (int_of_string s) }
  
  | '(' | "begin"          { LPAREN }
  | ')' | "end"            { RPAREN }
                           
  | "&&"                   { AND }
  | "||"                   { OR }
  | "not"                  { NOT }
  | "if"                   { IF }
  | "then"                 { THEN }
  | "else"                 { ELSE }
  | booleen as b           { BOOL(bool_of_string (b)) }
                           
  | "let"                  { LET }
  | "in"                   { IN }
  | ";;"                   { DBL_SCOLON }
  | "fun"                  { FUN }
  | "->"                   { MAPSTO }
  | "rec"                  { REC }

  | "ref"                  { REF }
  | ":="                   { REF_SET }
  | '!'                    { REF_GET }
  | ';'                    { SCOLON }
  
  | (lettre_minuscule | '_')+ (lettre | chiffre | '_' | '\'')* as s  { ID(s) }

  | eof                    { EOF }
    (* EndOfFile : ici on fait un "raise", plus tard votre fouine ne paniquera
       pas en tombant sur eof *)
