
all: ocamlweb-man.ps ocamlweb-man.html

WWW=$(HOME)/WWW/ocamlweb/
FTP=$(HOME)/ftp/ocamlweb/

export: all
	gzip -c ocamlweb-man.ps > $(FTP)/ocamlweb-man.ps.gz
	cp ocamlweb-man.html $(WWW)

ocamlweb-man.ps: ocamlweb-man.dvi
	dvips ocamlweb-man.dvi -o ocamlweb-man.ps

ocamlweb-man.dvi: ocamlweb-man.tex
	latex ocamlweb-man
	latex ocamlweb-man

ocamlweb-man.html: ocamlweb-man.tex
	hevea ocamlweb-man.tex

clean:
	rm -f *~ *.aux *.log *.dvi *.ps
