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

%nonassoc THEN

%right ELSE (*IN*)

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
  o=operation EOL {o}       /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */

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
 
sexpression: (*Tout ce qui peut-etre argument d'une fonction*)
  | LPAREN RPAREN                         { Id "Unit" }
    (*Majuscule pour ne pas confondre avec la variable qui s'appellerait "unit" (identifiant
      licite en Ocaml). Utilite : pour les fonctions sans parametres.*)
  | i=INT                                 { Const i }
  | b=BOOL                                { Bool b }
  | x=ID                                  { Id x }
  | LPAREN e=expression RPAREN            { e }
  (*| LPAREN f=fonction RPAREN              { f }*)
  (*Car une fonction peut-etre donnee en argument*)
(*  | LPAREN a=application RPAREN           { a }*)

parametre: (*ajouter enchainement de () ou alternance avec ID. Pas difficile mais pas la prio*)
  | LPAREN RPAREN                         { ["Unit"] }
  | LPAREN RPAREN p=parametre             { "Unit"::p }
  | x=ID                                  { [x] }
  | x=ID p=parametre                      { x::p }

fonction:
  | f=paren_fonct                         { f }
  | FUN p=parametre MAPSTO o=operation    { Fun(p,o) }                        

paren_fonct:
  | LPAREN f=fonction RPAREN              { f }

operation:
  | e=expression                          { e }
  | f=fonction                            { f }
  | a=application                         { a }
  | LET p=parametre EQUAL o1=operation
    IN o2=operation                       { Let_id_in(p,o1,o2) }

application:
  | LPAREN a=application RPAREN           { a }
    (*Pour que une une application parenthesee soit une application. 
      Mais double les lignes dessous... On ne peut pas le mettre ds sexpr 
      sinon deux chemins possibles (si on efface pas ici). Ou f (g h) n'est pas reconnu. *)
  | pf=paren_fonct s=sexpression           { App(pf,s) }
  | pf=paren_fonct LPAREN
    a=application RPAREN                  { App(pf,a) }
  | x=ID s=sexpression                    { App(Id x,s) }
  | a=application s=sexpression           { App(a,s) }
  | a1=application LPAREN a2=application
    RPAREN                                { App(a1, a2) }

  (*code duplique pour les fonctions parenthesees, solution sans dupli ?
    Pb etait fonct -> ( fonct ) concurrence avec sexpr -> ( fonct ) *)
  | pf1=paren_fonct pf2=paren_fonct       { App(pf1,pf2) }
  | x=ID pf=paren_fonct                   { App(Id x,pf) }
  | a=application pf=paren_fonct          { App(a,pf) }
