/* ON NE VEUT PAS VOIR CE COMMENTAIRE */

%{

%}

%token PLUS MOINS MULT DIV
%token <string> IDENT

%type <Ast.t> main
%start main

%%

