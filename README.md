# fouine

Ce projet part d'un squelette de base n'ayant que l'addition, la soustraction et la multiplication.

Dans ce projet, les nouvelles notions abordées sont :
    - Grammaire
    - Analyse lexicale
    - Analyse syntaxique
    - lambda calcul




README d'origine :


Ce répertoire contient un sequelette de départ à partir duquel vous
pouvez programmer votre fouine.

## Compilation et execution

pour (re)compiler, lancer
```
dune build
```

pour compiler et executer le programme, lancer
```
dune exec ./main.exe
```

pour executer le programme, lancer
```
./_build/default/main.exe
```

entrez ensuite une expression arithmetique, avec juste `+` et `*`, comme par exemple `4+3*5`
et vous obtiendrez:
```
Add(4, Mul(3, 5))
19
```

vous pouvez aussi faire:
```
dune exec ./main.exe < tests/basic.ml
```
pour lancer fouine sur le fichier `basic.ml`

main.ml : fichier principal
expr.ml : définition des expressions et de l'évaluation
affichage.ml : fonctions d'affichage
lexer.mll : lexèmes, analyse lexicale
parser.mly : règles de grammaire, analyse syntaxique
tests/ : sous-répertoire de tests
dune, dune-project : pour la compilation, à ne pas modifier a priori

## Erreurs à la compilation en lien avec le lexer et le parser :
   référez-vous à l'archive disponible depuis la page du portail des études

