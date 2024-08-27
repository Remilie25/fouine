%{
(* --- préambule: ici du code Caml --- *)

open Types

%}





/* liste des lexèmes (lien avec lexer.mll) */                                   
%token <int> INT       /* le lexème INT a un attribut entier */
%token PLUS TIMES MINUS DIV EQUAL GREATER
%token <bool> BOOL
%token AND OR NOT IF THEN ELSE
%token <string> ID
%token LET IN FUN MAPSTO REC
%token LPAREN RPAREN
%token DBL_SCOLON EOF


/* les associativités */

%nonassoc THEN MAPSTO IN DBL_SCOLON

%right ELSE

%nonassoc EQUAL GREATER

%left PLUS MINUS OR AND  /* associativité gauche: a+b+c, c'est (a+b)+c */
   /* associativité gauche ; priorité plus grande de TIMES par rapport à
      PLUS et MINUS, car est sur une ligne située plus bas */
                       
%left TIMES DIV

%nonassoc NOT

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
  | e=expression DBL_SCOLON                                    { [e] }
  | e=expression DBL_SCOLON t=top_expression                   { e::t }
  | l=let_surface DBL_SCOLON                                   { [l] }
  | l=let_surface DBL_SCOLON t=top_expression                  { l::t } 

let_surface:
  | LET p=parametre EQUAL e=expression                         { Let_id(Non_rec,p,e) }
  | LET REC p=parametre EQUAL e=expression                     { Let_id(Rec(string_of_id(List.hd p)),p,e) }
                                
expression:			    /* règles de grammaire pour les expressions */
  | s=sexpression                                              { s }
  | a=application                                              { a }
  | e1=expression PLUS e2=expression                           { App(App(Binary_op Add, e1), e2) }
  | e1=expression TIMES e2=expression                          { App(App(Binary_op Mul, e1), e2) }
  | e1=expression MINUS e2=expression                          { App(App(Binary_op Min, e1), e2) }
  | MINUS e=expression %prec UMINUS                            { App(App(Binary_op Min, Const 0), e) }
  | e1=expression DIV e2=expression                            { App(App(Binary_op Div, e1), e2) }
  | e1=expression EQUAL e2=expression                          { App(App(Binary_op Eq, e1), e2) }
  | e1=expression GREATER e2=expression                        { App(App(Binary_op Grt, e1), e2) }
                                                               
  | e1=expression AND e2=expression                            { App(App(Binary_op And, e1), e2) }
  | e1=expression OR e2=expression                             { App(App(Binary_op Or, e1), e2) }
  | NOT e=expression                                           { App(Unary_op Not, e) }
  | IF e1=expression THEN e2=expression                        { If_then(e1,e2) }
  | IF e1=expression THEN e2=expression ELSE e3=expression     { If_then_else(e1,e2,e3) }
                                                               
  | FUN p=parametre MAPSTO e=expression                        { Fun(Non_rec,p,e) }
  | LET p=parametre EQUAL e1=expression IN e2=expression       { Let_id_in(Non_rec,p,e1,e2) }
  | LET REC p=parametre EQUAL e1=expression IN e2=expression   { Let_id_in(Rec(string_of_id(List.hd p)),p,e1,e2) }
                                                               
                                                               
sexpression: (*Tout ce qui peut-etre argument d'une fonction*)
  | LPAREN RPAREN                                              { Expr_unit }
  | i=INT                                                      { Const i }
  | b=BOOL                                                     { Bool b }
  | x=ID                                                       { Id x }
  | LPAREN e=expression RPAREN                                 { e }
                                                               
                                                               
parametre:
  | LPAREN RPAREN                                              { [Expr_unit] }
  | LPAREN RPAREN p=parametre                                  { Expr_unit::p }
  | x=ID                                                       { [Id x] }
  | x=ID p=parametre                                           { (Id x)::p }


application:                                                   
  | s1=sexpression s2=sexpression                              { App(s1,s2) }
  | a=application s=sexpression                                { App(a,s) }

