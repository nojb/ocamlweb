
NAME=ocamlweb-$(VERSION)
PSDOC=$(NAME)-man.ps
HTMLDOC=$(NAME)-man.html

all: $(PSDOC) $(HTMLDOC)

WWW=$(HOME)/WWW/ocamlweb/
FTP=$(HOME)/ftp/ocamlweb/

export: all
	gzip -c $(PSDOC) > $(FTP)/$(PSDOC).gz
	cp $(HTMLDOC) $(WWW)

$(PSDOC): ocamlweb-man.dvi
	dvips $^ -o $@

ocamlweb-man.dvi: ocamlweb-man.tex
	latex $^
	latex $^

$(HTMLDOC): ocamlweb-man.tex
	hevea $^ -o $@

clean:
	rm -f *~ *.aux *.log *.dvi *.ps *.toc
	rm -f $(HTMLDOC)