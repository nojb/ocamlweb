rm -f *.tex *.log *.toc *.aux
make -C ..
../ocamlweb $1.$2 -o $1.tex 2> $1.err
latex $1.tex
latex $1.tex
xdvi $1.dvi
