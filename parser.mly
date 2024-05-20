%{
(* --- préambule: ici du code Caml --- *)

open Types   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

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

%nonassoc THEN

%right ELSE IN

%nonassoc EQUAL GREATER

%left PLUS MINUS OR AND  /* associativité gauche: a+b+c, c'est (a+b)+c */
   /* associativité gauche ; priorité plus grande de TIMES par rapport à
      PLUS et MINUS, car est sur une ligne située plus bas */

%left TIMES DIV

%nonassoc NOT

%nonassoc UMINUS  /* un "faux token", que l'on utilise pour le MINUS unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */

		    
%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Types.expr> main     /* on _doit_ donner le type associé au point d'entrée */



%%
    /* --- règles de grammaire --- */
main:                       /* <- le point d'entrée (cf. + haut, "start") */
  e=expression EOL { e }  /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */
  | f=fonction EOL { f }
  | a=application EOL { a }

expression:			    /* règles de grammaire pour les expressions */
  | s=sexpression                         { s }
  | e1=expression PLUS e2=expression      { Add(e1,e2) }
  | e1=expression TIMES e2=expression     { Mul(e1,e2) }
  | e1=expression MINUS e2=expression     { Min(e1,e2) }
  | MINUS e=expression %prec UMINUS       { Min(Const 0, e) }
  | e1=expression DIV e2=expression       { Div(e1,e2) }
  | e1=expression EQUAL e2=expression     { Eq(e1,e2) }
  | e1=expression GREATER e2=expression   { Grt(e1,e2) }

  | e1=expression AND e2=expression       { And(e1,e2) }
  | e1=expression OR e2=expression        { Or(e1,e2) }
  | NOT e1=expression                     { Not(e1) }
  | IF e1=expression THEN e2=expression   { If_then(e1,e2) }
  | IF e1=expression THEN e2=expression
    ELSE e3=expression                    { If_then_else(e1,e2,e3) }

  | LET e1=expression EQUAL
    e2=expression
    IN e3=expression                      { Let_id_in(e1,e2,e3) }
  
  (*| LET p=parametre EQUAL o=operation
    IN e=expression                       { Let_fun_in(p,o,e) }*)
            
 
sexpression:
  | LPAREN RPAREN                         { Id "Unit" }
    (*Majuscule pour ne pas confondre avec la variable qui s'appellerait "unit" (identifiant
      licite en Ocaml). Utilite : pour les fonctions sans parametres.*)
  | i=INT                                 { Const i }
  | b=BOOL                                { Bool b }
  | x=ID                                  { Id x }
  | LPAREN e=expression RPAREN            { e }
  | LPAREN f=fonction RPAREN              { f }
  | LPAREN a=application RPAREN           { a }

parametre: (*ajouter enchainement de () ou alternance avec ID. Pas difficile mais pas la prio*)
  | LPAREN RPAREN                         { ["Unit"] }
  | x=ID                                  { [x] }
  | x=ID p=parametre                      { x::p }

fonction:
  | FUN p=parametre MAPSTO o=operation    { Fun(p,o) }                        

operation:
  | e=expression                          { e }
  | f=fonction                            { f }
  | a=application                         { a }

application:
  (*| LPAREN a=application RPAREN           { a } reduce/recduce conflict; besoin d'une solution
    pour que une une application parenthesee soit une application.*)
  | LPAREN f=fonction RPAREN s=sexpression{ App(f,s) }
  | x=ID s=sexpression                    { App(Id x,s) }
  | a=application s=sexpression           { App(a,s) }

