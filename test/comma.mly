%{

(*s test/comma.mly

This ocamlyacc file demonstrates that the original 
yacc parser written in C used by ocamlyacc
accepts the comma as whitespace.
yacc_lexer.mll should also. *)

open Ast

%}

%token OR, XOR
%start file
%type <Ast.directive list> file 


%%
file: OR {}
| XOR {}
%%
