#########################################
# Configuration part : where to install
#########################################

BINDIR = $(HOME)/bin/$(OSTYPE)

#########################################
# End of configuration part
#########################################

MAJORVN=0
MINORVN=15
VERSION=$(MAJORVN).$(MINORVN)

CAMLC    = ocamlc
CAMLCOPT = ocamlopt 
CAMLDEP  = ocamldep
ZLIBS    =
DEBUG    = -g
PROFILE  =
BYTEFLAGS= $(ZLIBS) $(DEBUG)
OPTFLAGS = $(ZLIBS) $(PROFILE)

BUFFER = buffer.cmx
OBJS = $(BUFFER) output.cmx cross.cmx pretty.cmx web.cmx lexer.cmx \
       version.cmx main.cmx

all: ocamlweb

ocamlweb: $(OBJS:.cmx=.cmo)
	$(CAMLC) $(BYTEFLAGS) -o ocamlweb $(OBJS:.cmx=.cmo)

opt: $(OBJS)
	$(CAMLCOPT) $(OPTFLAGS) -o ocamlweb $(OBJS)

debug: $(OBJS:.cmx=.cmo)
	$(CAMLC) $(BYTEFLAGS) -o ocamlweb-debug $(OBJS:.cmx=.cmo)

version.ml: Makefile
	echo "let version = \""$(VERSION)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

install:
	cp ocamlweb $(BINDIR)

byte: $(OBJS:.cmx=.cmo)

manual:
	cd doc; make all

test: ocamlweb
	cd tmp; ../ocamlweb foo.ml essai.ml ../main.ml ../output.mli ../web.ml -o essai.tex ; \
	latex essai

# export
########

NAME=ocamlweb-$(VERSION)

FTP = /users/demons/filliatr/ftp/ocaml/ocamlweb

FILES = buffer.mli buffer.ml \
        lexer.mll cross.mll pretty.mli pretty.mll \
	output.mli output.ml web.mli web.ml main.ml \
	Makefile .depend README INSTALL COPYING GPL CHANGES

export: source
	cp README COPYING GPL CHANGES  $(FTP)
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
	rm -f *~ *.cm[iox] *.o ocamlweb lexer.ml pretty.ml cross.ml version.ml

depend: lexer.ml pretty.ml cross.ml
	rm -f .depend
	ocamldep $(ZLIBS) *.mli *.ml > .depend

include .depend

