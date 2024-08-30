%{
(* --- préambule: ici du code Caml --- *)

open Types

%}





/* liste des lexèmes (lien avec lexer.mll) */                                   
%token <int> INT       /* le lexème INT a un attribut entier */
%token PLUS TIMES MINUS DIV EQUAL GREATER LESS
%token <bool> BOOL
%token AND OR NOT IF THEN ELSE
%token <string> ID
%token LET IN FUN MAPSTO REC
%token LPAREN RPAREN
%token DBL_SCOLON EOF
%token REF REF_SET REF_GET SCOLON


/* les associativités */
%nonassoc EXPR_TO_SEQ
     
%right SCOLON
     
%nonassoc THEN

%right ELSE REF_SET REF_GET

%nonassoc EQUAL GREATER LESS

%left PLUS MINUS OR AND  /* associativité gauche: a+b+c, c'est (a+b)+c */
   /* associativité gauche ; priorité plus grande de TIMES par rapport à
      PLUS et MINUS, car est sur une ligne située plus bas */
                       
%left TIMES DIV

%nonassoc UMINUS /* un "faux token", que l'on utilise pour le MINUS unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */

		    
%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Types.expr list> main     /* on _doit_ donner le type associé au point d'entrée */



%%
    /* --- règles de grammaire --- */
main:                       /* <- le point d'entrée (cf. + haut, "start") */
  t=top_expression EOF                                         { t }

top_expression:
  | e=expression_seq DBL_SCOLON                                { [e] }
  | e=expression_seq DBL_SCOLON t=top_expression               { e::t }
  | l=let_surface DBL_SCOLON                                   { [l] }
  | l=let_surface DBL_SCOLON t=top_expression                  { l::t }

expression_seq:
  | e=expression %prec EXPR_TO_SEQ                             { e }
  | e=expression SCOLON seq=expression_seq                     { seq_add e seq }

let_surface:
  | LET p=parametre EQUAL e=expression_seq                     { Let_id(Non_rec,p,e) }
  | LET REC p=parametre EQUAL e=expression_seq                 { Let_id(Rec(string_of_id(List.hd p)),p,e) }
                                
expression:			    /* règles de grammaire pour les expressions */
  | s=sexpression                                              { s }
  | a=application                                              { a }
  
  | e1=expression PLUS e2=expression                           { App(App(Binary_op Add, e1), e2) }
  | e1=expression TIMES e2=expression                          { App(App(Binary_op Mul, e1), e2) }
  | e1=expression MINUS e2=expression                          { App(App(Binary_op Min, e1), e2) }
  | MINUS e=expression %prec UMINUS                            { App(App(Binary_op Min, Const 0), e) }
  | e1=expression DIV e2=expression                            { App(App(Binary_op Div, e1), e2) }
  | e1=expression EQUAL e2=expression                          { App(App(Binary_op Eq, e1), e2) }
  | e1=expression LESS GREATER e2=expression                   { App(App(Binary_op Neq, e1), e2) }
  | e1=expression GREATER e2=expression                        { App(App(Binary_op Grt, e1), e2) }
  | e1=expression GREATER EQUAL e2=expression                  { App(App(Binary_op Geq, e1), e2) }
  | e1=expression LESS e2=expression                           { App(App(Binary_op Less, e1), e2) }
  | e1=expression LESS EQUAL e2=expression                     { App(App(Binary_op Leq, e1), e2) }
                                                               
  | e1=expression AND e2=expression                            { App(App(Binary_op And, e1), e2) }
  | e1=expression OR e2=expression                             { App(App(Binary_op Or, e1), e2) }
  | IF e1=expression_seq THEN e2=expression                    { If_then(e1,e2) }
  | IF e1=expression_seq THEN e2=expression ELSE e3=expression { If_then_else(e1,e2,e3) }
                                                               
  | FUN p=parametre MAPSTO e=expression_seq                    { Fun(Non_rec,p,e) }
  | l=let_surface IN seq=expression_seq                        { build_let_id_in l seq }

  | e1=expression REF_SET e2=expression                        { App(App(Binary_op Ref_set, e1), e2) }
  | REF_GET e=expression                                       { App(Unary_op Ref_get, e) }
                                                               
                                                               
sexpression: (*Tout ce qui peut-etre argument d'une fonction*)
  | LPAREN RPAREN                                              { Expr_unit }
  | i=INT                                                      { Const i }
  | b=BOOL                                                     { Bool b }
  | x=ID                                                       { Id x }
  | LPAREN e=expression_seq RPAREN                             { e }

  (*Fonction definie de base*)
  | LPAREN PLUS RPAREN                                         { Binary_op Add }
  | LPAREN TIMES RPAREN                                        { Binary_op Mul }
  | LPAREN MINUS RPAREN                                        { Binary_op Min }
  | LPAREN DIV RPAREN                                          { Binary_op Div }
  | LPAREN EQUAL RPAREN                                        { Binary_op Eq }
  | LPAREN LESS GREATER RPAREN                                 { Binary_op Neq }
  | LPAREN GREATER RPAREN                                      { Binary_op Grt }
  | LPAREN GREATER EQUAL RPAREN                                { Binary_op Geq }
  | LPAREN LESS RPAREN                                         { Binary_op Less }
  | LPAREN LESS EQUAL RPAREN                                   { Binary_op Leq }
  
  | LPAREN OR RPAREN                                           { Binary_op Or }
  | LPAREN AND RPAREN                                          { Binary_op And }
  | NOT                                                        { Unary_op Not }
  
  | REF                                                        { Unary_op Ref }
  | LPAREN REF_SET RPAREN                                      { Binary_op Ref_set }
  | LPAREN REF_GET RPAREN                                      { Unary_op Ref_get }
                                                               
parametre:
  | LPAREN RPAREN                                              { [Expr_unit] }
  | LPAREN RPAREN p=parametre                                  { Expr_unit::p }
  | x=ID                                                       { [Id x] }
  | x=ID p=parametre                                           { (Id x)::p }


application:                                                   
  | s1=sexpression s2=sexpression                              { App(s1,s2) }
  | a=application s=sexpression                                { App(a,s) }
