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
%token LET IN FUN MAPSTO
%token LPAREN RPAREN
%token EOL             /* retour à la ligne */


/* les associativités */

%nonassoc THEN MAPSTO IN

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
%type <Types.expr> main     /* on _doit_ donner le type associé au point d'entrée */



%%
    /* --- règles de grammaire --- */
main:                       /* <- le point d'entrée (cf. + haut, "start") */
  e=expression EOL {e}       /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */

expression:			    /* règles de grammaire pour les expressions */
  | s=sexpression                                           { s }
  | a=application {a} 
  | e1=expression PLUS e2=expression                        { Add(e1,e2) }
  | e1=expression TIMES e2=expression                       { Mul(e1,e2) }
  | e1=expression MINUS e2=expression                       { Min(e1,e2) }
  | MINUS e=expression %prec UMINUS                         { Min(Const 0, e) }
  | e1=expression DIV e2=expression                         { Div(e1,e2) }
  | e1=expression EQUAL e2=expression                       { Eq(e1,e2) }
  | e1=expression GREATER e2=expression                     { Grt(e1,e2) }
                                                            
  | e1=expression AND e2=expression                         { And(e1,e2) }
  | e1=expression OR e2=expression                          { Or(e1,e2) }
  | NOT e1=expression                                       { Not(e1) }
  | IF e1=expression THEN e2=expression                     { If_then(e1,e2) }
  | IF e1=expression THEN e2=expression ELSE e3=expression  { If_then_else(e1,e2,e3) }
                                                            
  | FUN p=parametre MAPSTO e=expression                     { Fun(p,e) }
  | LET p=parametre EQUAL e1=expression IN e2=expression    { Let_id_in(p,e1,e2) }
                                                            
                                                            
sexpression: (*Tout ce qui peut-etre argument d'une fonction*)
  | LPAREN RPAREN                                           { Expr_unit }
  | i=INT                                                   { Const i }
  | b=BOOL                                                  { Bool b }
  | x=ID                                                    { Id x }
  | LPAREN e=expression RPAREN                              { e }
 
                                                            
parametre:
  | LPAREN RPAREN                                           { [Expr_unit] }
  | LPAREN RPAREN p=parametre                               { Expr_unit::p }
  | x=ID                                                    { [Id x] }
  | x=ID p=parametre                                        { (Id x)::p }
                                                                                                                        
application:                                                
  | s1=sexpression s2=sexpression                           { App(s1,s2) }
  | a=application s=sexpression                             { App(a,s) }

