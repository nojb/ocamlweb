#########################################
# Configuration part : where to install
#########################################

BINDIR = $(HOME)/bin/$(OSTYPE)

#########################################
# End of configuration part
#########################################

MAJORVN=0
MINORVN=04
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

ocamlweb: $(OBJS)
	$(CAMLCOPT) $(OPTFLAGS) -o ocamlweb $(OBJS)

debug: $(OBJS:.cmx=.cmo)
	$(CAMLC) $(BYTEFLAGS) -o ocamlweb-debug $(OBJS:.cmx=.cmo)

version.ml: Makefile
	echo "let version = \""$(VERSION)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

install:
	cp ocamlweb $(BINDIR)

byte: $(OBJS:.cmx=.cmo)

test: ocamlweb
	cd tmp; ../ocamlweb ../output.mli ../web.ml -o essai.tex ; \
	latex essai

# export
########

NAME=ocamlweb-$(VERSION)

FTP = /users/demons/filliatr/ftp/ocaml/ocamlweb

FILES = lexer.mll main.ml Makefile .depend README COPYING GPL CHANGES

export: source linux solaris

source: $(FILES)
	mkdir -p export/$(NAME)
	cp $(FILES) export/$(NAME)
	(cd export ; tar cf $(NAME).tar ocamlweb ; \
	gzip -f --best $(NAME).tar)
	cp README COPYING GPL CHANGES export/$(NAME).tar.gz $(FTP)

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

