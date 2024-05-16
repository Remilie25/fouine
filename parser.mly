%{
(* --- préambule: ici du code Caml --- *)

(*open Types   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)*)

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
%nonassoc EQUAL GREATER (*FUN*) MAPSTO
%left VAPPLY APPLY
%nonassoc ID
%left PLUS MINUS OR AND  /* associativité gauche: a+b+c, c'est (a+b)+c */
   /* associativité gauche ; priorité plus grande de TIMES par rapport à
      PLUS et MINUS, car est sur une ligne située plus bas */

%left TIMES DIV
                       
%nonassoc NOT (*IF*) (*THEN*) ELSE (*LET*) IN
%nonassoc UMINUS  /* un "faux token", que l'on utilise pour le MINUS unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */

		    
%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Types.expr> main     /* on _doit_ donner le type associé au point d'entrée */



%%
    /* --- règles de grammaire --- */
main:                       /* <- le point d'entrée (cf. + haut, "start") */
  e=expression EOL { e }  /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */
  |s=sexpr EOL {s}
  |a=application EOL { a }

expression:			    /* règles de grammaire pour les expressions */
  | i=INT                        { Const i } 
      /* on appelle i l'attribut associé à INT */
      /* les "let in" sont juste là pour illustrer le fait que l'on peut mettre
         du code Caml dans les parties entre {..} ; supprimez-les pour
         montrer que vous avez lu ceci, et mettez juste "Const i" */
  | e1=expression PLUS e2=expression      { Add(e1,e2) }
  | e1=expression TIMES e2=expression     { Mul(e1,e2) }
  | e1=expression MINUS e2=expression     { Min(e1,e2) }
  | MINUS e=expression %prec UMINUS       { Min(Const 0, e) }
  | e1=expression DIV e2=expression       { Div(e1,e2) }
  | e1=expression EQUAL e2=expression     { Eq(e1,e2) }
  | e1=expression GREATER e2=expression   { Grt(e1,e2) }
  
  | b=BOOL                                { Bool b }
  | e1=expression AND e2=expression       { And(e1,e2) }
  | e1=expression OR e2=expression        { Or(e1,e2) }
  | NOT e1=expression                     { Not(e1) }
  | IF e1=expression THEN e2=expression
    ELSE e3=expression                    { If_then_else(e1,e2,e3) }

  | x=ID                                  { Id x }
  | LET e1=expression EQUAL
    e2=expression
    IN e3=expression                      { Let_id_in(e1,e2,e3) }
  (*| LET e1=expression e2=expression EQUAL
                           e3=expression IN e4=expression        { Fun(e1,e2,e3,e4) }*)
 
sexpr:                                  
  | LPAREN e1=expression RPAREN  { e1 }

application:
  | LPAREN a=application RPAREN           { a }
  | FUN e1=expression MAPSTO e2=expression{ Fun(e1,e2) }
  (*| FUN e1=expression MAPSTO e2=sexpr     { Fun(e1,e2) }*)
  | x=ID e=expression %prec VAPPLY              { App(Id x,e) }
  (*| x=ID e=sexpr                    { App(Id x,e) }*)
  | a=application e=expression %prec APPLY{ App(a,e) }
  | a=application s=sexpr                 { App(a,s) }
  

