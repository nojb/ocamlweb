
all: ocamlweb-manual.ps

WWW=$(HOME)/ftp/ocamlweb/

export:
	cp ocamlweb-manual.ps $(WWW)

ocamlweb-manual.ps: ocamlweb-manual.dvi
	dvips ocamlweb-manual.dvi -o ocamlweb-manual.ps

ocamlweb-manual.dvi: ocamlweb-manual.tex
	latex ocamlweb-manual

clean:
	rm -f *~ *.aux *.log *.dvi *.ps
