
open Expr

let interpret e =
  begin
    affiche_expr e;
    print_newline();
    Types.print_valeur (eval e []);
    print_newline()
  end

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let calc () =
  let result = parse () in
  interpret result; flush stdout

let _ = calc()
