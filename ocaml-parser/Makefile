
CAML=/users/demons/filliatr/local/src/ocaml-2.02

CAMLC    = ocamlc
CAMLCOPT = ocamlopt 
CAMLDEP  = ocamldep
ZLIBS    =
DEBUG    = -g
PROFILE  =
BYTEFLAGS= $(ZLIBS) $(DEBUG)
OPTFLAGS = $(ZLIBS) $(PROFILE)

SOURCES = utils/config.mli utils/misc.mli utils/misc.ml \
	utils/clflags.mli utils/clflags.ml \
	utils/terminfo.mli utils/terminfo.ml \
	utils/warnings.mli utils/warnings.ml \
	parsing/linenum.mli parsing/linenum.mll \
	parsing/location.mli parsing/location.ml \
	parsing/longident.mli parsing/longident.ml \
	parsing/pstream.mli parsing/pstream.ml \
	parsing/syntaxerr.mli parsing/syntaxerr.ml \
	parsing/asttypes.mli parsing/parsetree.mli \
	parsing/parser.mly \
	parsing/lexer.mli parsing/lexer.mll \
	parsing/parse.mli parsing/parse.ml

OBJS = config.cmo \
       misc.cmo clflags.cmo terminfo.cmo warnings.cmo \
       linenum.cmo location.cmo longident.cmo pstream.cmo syntaxerr.cmo \
       parser.cmo lexer.cmo parse.cmo

all: $(OBJS)
	ocamlc -o ocaml-parser $(OBJS)

import:
#	config
	cd $(caml); ./configure; make utils/config.ml
	cp $(caml)/utils/config.ml .; chmod u+w config.ml
#	source files
	for f in $(SOURCES); do cp $(caml)/$$f .; done

linenum.ml: linenum.mll
	ocamllex linenum.mll

parser.mli parser.ml: parser.mly
	ocamlyacc -v parser.mly

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
	rm -f *~ *.cm[iox] *.o 

depend:
	rm -f .depend
	ocamldep $(ZLIBS) *.mli *.ml > .depend

include .depend

