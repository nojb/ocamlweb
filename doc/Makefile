
all: ocamlweb-man.ps

WWW=$(HOME)/ftp/ocamlweb/

export: all
	gzip -c ocamlweb-man.ps > $(WWW)/ocamlweb-man.ps.gz

ocamlweb-man.ps: ocamlweb-man.dvi
	dvips ocamlweb-man.dvi -o ocamlweb-man.ps

ocamlweb-man.dvi: ocamlweb-man.tex
	latex ocamlweb-man
	latex ocamlweb-man

clean:
	rm -f *~ *.aux *.log *.dvi *.ps
