#########################################
# Configuration part : where to install
#########################################

BINDIR = $(HOME)/bin/$(OSTYPE)

#########################################
# End of configuration part
#########################################

MAJORVN=0
MINORVN=2
VERSION=$(MAJORVN).$(MINORVN)

CAMLC    = ocamlc
CAMLCOPT = ocamlopt 
CAMLDEP  = ocamldep
ZLIBS    = -I ocaml-parser
DEBUG    = -g
PROFILE  =
BYTEFLAGS= $(ZLIBS) $(DEBUG)
OPTFLAGS = $(ZLIBS) $(PROFILE)

CAML_CMO = misc.cmo clflags.cmo terminfo.cmo warnings.cmo \
       linenum.cmo location.cmo longident.cmo pstream.cmo syntaxerr.cmo \
       parser.cmo lexer.cmo parse.cmo

CAML_CMX = $(CAML_CMO:.cmo=.cmx)

BUFFER = buffer.cmo
CMO = $(BUFFER) output.cmo cross.cmo pretty.cmo web.cmo doclexer.cmo \
       version.cmo main.cmo

CMX = $(CMO:.cmo=.cmx)

all: ocamlweb

ocamlweb: ocaml-parser-byte $(CMO)
	$(CAMLC) $(BYTEFLAGS) -o ocamlweb $(CAML_CMO) $(CMO)

opt: ocaml-parser-opt $(CMX)
	$(CAMLCOPT) $(OPTFLAGS) -o ocamlweb $(CAML_CMX) $(CMX)

debug: ocaml-parser-byte $(CMO)
	$(CAMLC) $(BYTEFLAGS) -o ocamlweb-debug $(CAML_CMO) $(CMO)

ocaml-parser-byte:
	cd ocaml-parser; make byte

ocaml-parser-opt:
	cd ocaml-parser; make opt

version.ml: Makefile
	echo "let version = \""$(VERSION)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

install:
	cp ocamlweb $(BINDIR)

manual:
	cd doc; make all

test: ocamlweb
	cd tmp; ../ocamlweb --no-web --latex-option CiME essai.ml ../output.ml ../web.ml -o essai.tex ; \
	latex essai ; latex essai

# export
########

NAME=ocamlweb-$(VERSION)

FTP = /users/demons/filliatr/ftp/ocaml/ocamlweb

FILES = buffer.mli buffer.ml \
        doclexer.mll cross.ml pretty.mli pretty.mll \
	output.mli output.ml web.mli web.ml main.ml \
	ocamlweb.sty \
	Makefile .depend README INSTALL COPYING GPL CHANGES

export: source
	cp README COPYING GPL CHANGES $(FTP)
	cd doc; make all export

source:
	mkdir -p export/$(NAME)
	cp $(FILES) export/$(NAME)
	(cd export ; tar cf $(NAME).tar $(NAME) ; \
	gzip -f --best $(NAME).tar)
	cp export/$(NAME).tar.gz $(FTP)

BINARY = $(NAME)-$(OSTYPE)

linux: clean binary

solaris:
	rmake sun-demons $(HOME)/soft/ocaml/ocamlweb clean binary

binary: ocamlweb
	mkdir -p export/$(BINARY)
	cp README COPYING GPL ocamlweb export/$(BINARY)
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


# clean and depend
##################

clean:
	rm -f *~ *.cm[iox] *.o ocamlweb doclexer.ml pretty.ml version.ml
	make -C ocaml-parser clean
	make -C doc clean

depend: doclexer.ml pretty.ml
	rm -f .depend
	ocamldep $(ZLIBS) *.mli *.ml > .depend
	make -C ocaml-parser depend

include .depend

