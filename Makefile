#########################################
# Configuration part : where to install
#########################################

# where to install the binary executable
BINDIR = /usr/local/bin

# where to put the style file
TEXDIR = /usr/share/texmf/tex/latex/misc

#########################################
# End of configuration part
#########################################

MAJORVN=0
MINORVN=7
VERSION=$(MAJORVN).$(MINORVN)

CAMLC    = ocamlc
CAMLCOPT = ocamlopt 
CAMLDEP  = ocamldep
ZLIBS    = -I ocaml-parser
DEBUG    = 
PROFILE  =
BYTEFLAGS= $(ZLIBS) $(DEBUG)
OPTFLAGS = $(ZLIBS) $(PROFILE)

CAML_CMO = ocaml-parser/misc.cmo ocaml-parser/clflags.cmo	\
	   ocaml-parser/terminfo.cmo ocaml-parser/warnings.cmo	\
           ocaml-parser/linenum.cmo ocaml-parser/location.cmo	\
	   ocaml-parser/longident.cmo ocaml-parser/pstream.cmo	\
           ocaml-parser/syntaxerr.cmo ocaml-parser/parser.cmo	\
           ocaml-parser/lexer.cmo ocaml-parser/parse.cmo

CAML_CMX = $(CAML_CMO:.cmo=.cmx)

CMO = output.cmo cross.cmo pretty.cmo web.cmo doclexer.cmo \
      version.cmo main.cmo

CMX = $(CMO:.cmo=.cmx)

all: ocamlweb

ocamlweb: $(CAML_CMX) $(CMX)
	$(CAMLCOPT) $(OPTFLAGS) -o ocamlweb $(CAML_CMX) $(CMX)
	strip ocamlweb

opt: ocamlweb

byte: $(CAML_CMO) $(CMO)
	$(CAMLC) $(BYTEFLAGS) -o ocamlweb $(CAML_CMO) $(CMO)

debug: $(CAML_CMO) $(CMO)
	$(CAMLC) $(BYTEFLAGS) -o ocamlweb-debug $(CAML_CMO) $(CMO)

version.ml: Makefile
	echo "let version = \""$(VERSION)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

install:
	cp ocamlweb $(BINDIR)
	cp ocamlweb.sty $(TEXDIR)

manual:
	cd doc; make all

LATEX=TEXINPUTS=..: ; export TEXINPUTS ; latex

test: ocamlweb
	cd tmp; ../ocamlweb --latex-sects essai.ml -o essai.tex ; \
	$(LATEX) essai ; grep -q "Rerun" essai.log && $(LATEX) essai || true

BOOTSTRAP= bootstrap.tex output.mli output.ml cross.mli cross.ml \
           pretty.mli --impl pretty.mll web.mli web.ml \
	   doclexer.mli --impl doclexer.mll main.ml 

bootstrap: ocamlweb
	./ocamlweb -o test/ocamlweb.tex $(BOOTSTRAP)
	cd test; $(LATEX) ocamlweb
	cd test; grep -q "Rerun" ocamlweb.log && $(LATEX) ocamlweb || true
	-cd test; hevea -o ocamlweb.html ../ocamlweb.sty ocamlweb.tex
	cd test; dvips ocamlweb.dvi -o ocamlweb.ps

check: bootstrap

# export
########

NAME=ocamlweb-$(VERSION)

FTP = /users/demons/filliatr/ftp/ocaml/ocamlweb

FILES = doclexer.mli doclexer.mll cross.ml cross.mli		\
	pretty.mli pretty.mll					\
	output.mli output.ml web.mli web.ml main.ml		\
	ocamlweb.sty bootstrap.tex				\
	Makefile .depend README INSTALL COPYING GPL CHANGES

OCAMLFILES = misc.mli misc.ml clflags.ml	\
        terminfo.mli terminfo.ml		\
	warnings.mli warnings.ml		\
	linenum.mli linenum.mll			\
	location.mli location.ml		\
	longident.mli longident.ml		\
	pstream.mli pstream.ml			\
	syntaxerr.mli syntaxerr.ml		\
	asttypes.mli parsetree.mli		\
	parser.mly				\
	lexer.mli lexer.mll			\
	parse.mli parse.ml			\
	README

export: source linux
	cp README COPYING GPL CHANGES $(FTP)
	cd doc; make all export
	make export-bootstrap

export-bootstrap: bootstrap
	gzip -c test/ocamlweb.ps > $(FTP)/ocamlweb.ps.gz
	cp test/ocamlweb.html $(FTP)

source:
	mkdir -p export/$(NAME)/test
	cd export/$(NAME); mkdir -p ocaml-parser; mkdir -p test
	cp $(FILES) export/$(NAME)
	cd ocaml-parser; cp $(OCAMLFILES) ../export/$(NAME)/ocaml-parser
	(cd export ; tar cf $(NAME).tar $(NAME) ; \
	gzip -f --best $(NAME).tar)
	cp export/$(NAME).tar.gz $(FTP)

BINARY = $(NAME)-$(OSTYPE)

linux: clean binary

solaris:
	rmake sun-demons $(HOME)/soft/ocaml/ocamlweb clean binary

BINARYFILES = README INSTALL COPYING GPL ocamlweb ocamlweb.sty

binary: ocamlweb
	mkdir -p export/$(BINARY)
	cp $(BINARYFILES) export/$(BINARY)
	(cd export; tar czf $(BINARY).tar.gz $(BINARY))
	cp export/$(BINARY).tar.gz $(FTP)

# generic rules :
#################

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll
 
.mll.ml:
	ocamllex $<
 
.mli.cmi:
	$(CAMLC) -c $(BYTEFLAGS) $<
 
.ml.cmo:
	$(CAMLC) -c $(BYTEFLAGS) $<

.ml.o:
	$(CAMLCOPT) -c $(OPTFLAGS) $<

.ml.cmx:
	$(CAMLCOPT) -c $(OPTFLAGS) $<

ocaml-parser/parser.mli ocaml-parser/parser.ml: ocaml-parser/parser.mly
	ocamlyacc -v ocaml-parser/parser.mly


# clean and depend
##################

clean:
	rm -f *~ *.cm[iox] *.o 
	rm -f ocamlweb doclexer.ml pretty.ml version.ml
	rm -f ocaml-parser/*~ ocaml-parser/*.cm[iox] ocaml-parser/*.o
	rm -f ocaml-parser/lexer.ml ocaml-parser/linenum.ml
	rm -f ocaml-parser/parser.mli ocaml-parser/parser.ml
	make -C doc clean

depend: doclexer.ml pretty.ml ocaml-parser/parser.ml ocaml-parser/lexer.ml \
        ocaml-parser/linenum.ml
	rm -f .depend
	ocamldep $(ZLIBS) *.mli *.ml ocaml-parser/*.ml ocaml-parser/*.mli > .depend

include .depend

